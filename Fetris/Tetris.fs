open System
open System.Windows.Forms
open System.Drawing
open System.Threading

type Square = Square

type Rotation =
    | Zero
    | Ninety
    | OneEighty
    | TwoSeventy 
    static member Next =
        function
        | Zero -> Ninety
        | Ninety -> OneEighty
        | OneEighty -> TwoSeventy
        | TwoSeventy -> Zero

type Tile =
    S | Z | L | J | T | O | I
    with
        static member All = [S; Z; L; J; T; O; I]

        static member project tile rotation =
            // Table built from: http://tetris.wikia.com/wiki/SRS
            match tile with
            | O ->
                match rotation with
                | Zero ->       [1,0; 2,0; 1,1; 2,1]
                | Ninety ->     [1,0; 2,0; 1,1; 2,1]
                | OneEighty ->  [1,0; 2,0; 1,1; 2,1]
                | TwoSeventy -> [1,0; 2,0; 1,1; 2,1]
            | J ->
                match rotation with
                | Zero ->       [0,0; 0,1; 1,1; 2,1]
                | Ninety ->     [1,0; 2,0; 1,1; 1,2]
                | OneEighty ->  [0,1; 1,1; 2,1; 2,2]
                | TwoSeventy -> [1,0; 1,1; 0,2; 1,2]
            | L ->
                match rotation with
                | Zero ->       [0,1; 1,1; 2,1; 2,0]
                | Ninety ->     [1,0; 1,1; 1,2; 2,2]
                | OneEighty ->  [0,1; 1,1; 2,1; 0,2]
                | TwoSeventy -> [0,0; 1,0; 1,1; 1,2]
            | I ->
                match rotation with
                | Zero ->       [0,1; 1,1; 2,1; 3,1]
                | Ninety ->     [2,0; 2,1; 2,2; 2,3]
                | OneEighty ->  [0,2; 1,2; 2,2; 3,2]
                | TwoSeventy -> [1,0; 1,1; 1,2; 1,3]
            | S ->
                match rotation with
                | Zero ->       [1,0; 2,0; 0,1; 1,1]
                | Ninety ->     [1,0; 1,1; 2,1; 2,2]
                | OneEighty ->  [1,1; 2,1; 0,2; 1,2]
                | TwoSeventy -> [0,0; 0,1; 1,1; 1,2]
            | Z ->
                match rotation with
                | Zero ->       [0,0; 1,0; 1,1; 2,1]
                | Ninety ->     [2,0; 1,1; 2,1; 1,2]
                | OneEighty ->  [0,1; 1,1; 1,2; 2,2]
                | TwoSeventy -> [1,0; 0,1; 1,1; 0,2]
            | T ->
                match rotation with
                | Zero ->       [1,0; 0,1; 1,1; 2,1]
                | Ninety ->     [1,0; 1,1; 2,1; 1,2]
                | OneEighty ->  [0,1; 1,1; 2,1; 1,2]
                | TwoSeventy -> [1,0; 0,1; 1,1; 1,2]            

type ActiveTile =
    {
        tile: Tile
        position: int*int
        rotation: Rotation
    } with static member Map {tile = tile; position = (x,y); rotation = rotation} =
            Tile.project tile rotation |> List.map(fun (tx,ty) -> tx+x,ty+y)

type Grid = {squares: Map<int*int,Square>; width: int; height: int}

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Grid =
    let create (width,height): Grid = {squares = Map.empty<int*int,Square>; width = width; height = height}

    let map {squares = grid} = grid

    let width {width = width} = width

    let height {height = height} = height

    let getSquare position = map >> Map.tryFind position

    let hasSquare position = getSquare position >> Option.isSome

    let addSquare square position ({squares = squares} as grid) = {grid with squares = squares |> Map.add position square}

    let removeSquare position ({squares = squares} as grid) = {grid with squares = squares |> Map.remove position}


type Input = Rotate | Hold | Drop | SpeedUp

type Game = {
    active: ActiveTile
    hold: Tile option
    holdLock: bool
    next: Tile
    grid: Grid
    score: int
    speed: float
    over: bool
}

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Game =

    let pieceGenerator =

        let random = Random()

        // Knuth shuffle
        let shuffle (list: 'a list) =
            let array = Array.ofList list             
            let Swap i j =                                                 
                let item = array.[i]
                array.[i] <- array.[j]
                array.[j] <- item
            let length = array.Length
            [0..(length - 2)]                                                   
            |> Seq.iter (fun i -> Swap i (random.Next(i, length)))                 
            array |> Array.toList                                                            

        MailboxProcessor<AsyncReplyChannel<Tile>>.Start(fun mailbox ->
            let rec generate bag = async {

                match bag with
                | head::tail ->
                    let! message = mailbox.Receive()
                    message.Reply head
                    return! generate tail
                | [] ->
                    return! Tile.All |> shuffle |> generate
            }

            generate []
        ) 
        |> (fun mailbox () -> mailbox.PostAndReply id)


    type Agent<'a> = Agent of MailboxProcessor<'a>

    type Reply<'a> = AsyncReplyChannel<'a>

    type Direction = Left | Right

    type StepType = Manual | Auto

    type Message = Step of StepType | State of Reply<Game> | Rotate | Move of Direction | Drop | Hold

    let start (width, height) =
        MailboxProcessor.Start(fun mailbox ->
            
            let startPosition = width/2 - 2, 0

            let stepIn speed =
                async {
                    do! Async.Sleep(750.0 / speed |> int)
                    mailbox.Post (Step Auto)
                } |> Async.Start

            let lock (game: Game) =
                let grid =
                    ActiveTile.Map game.active
                    |> List.fold (fun grid position -> grid |> Grid.addSquare Square position) game.grid
                {game with grid = grid}
                |> (fun game ->
                    if game.grid.squares |> Map.exists (fun (x,y) _ -> y <= 1) then
                        {game with over = true}
                    else
                        game
                )

            let collides ({active = active; grid = grid} as game) (x,y) =
                if x < 0 || x >= grid.width then true
                elif y >= grid.height then true
                elif grid |> Grid.hasSquare (x,y) then true
                else false

            let activeCollides ({active = active} as game) =
                ActiveTile.Map active |> List.exists (collides game)
                

            let clearRows ({grid = grid} as game) =
                
                let columns = {0..(grid.width-1)} |> List.ofSeq
                let rows = {0..(grid.height-1)}  |> List.ofSeq

                let isRowFilled grid y =
                    columns |> List.exists (fun x -> grid |> Grid.hasSquare (x,y) |> not) |> not
       
                let rec clearRow row (game: Game) =
                    if row >= game.grid.height then
                        game
                    elif isRowFilled game.grid row then
                        let newSquares =
                            game.grid.squares
                            |> Map.filter (fun (x,y) _ -> y <> row)
                            |> Map.toList
                            |> List.map (fun (((x,y), square) as item) ->
                                if y < row then (x,y+1), square else item
                            )
                            |> Map.ofList
                        {game with grid = {game.grid with squares = newSquares}}  
                        |> (fun game ->
                            {game with
                                speed = game.speed + 0.05
                                score = game.score + (game.speed * 300.0 |> int)
                            }
                        )
                        |> clearRow row

                    else
                        clearRow (row + 1) game
                                               

                clearRow 0 game


            let spawn game =
                {game with
                    active = {tile = game.next; position = startPosition; rotation = Zero}
                    holdLock = false
                    next = pieceGenerator ()}

            let hold (game: Game) =
                match game.hold with
                | Some tile when not game.holdLock ->
                    {game with active = {tile = tile; position = startPosition; rotation = Rotation.Zero}; hold = Some game.active.tile; holdLock = true}
                | None when not game.holdLock ->
                    {spawn game with hold = Some game.active.tile; holdLock = true}
                | _ -> game

            let rotate ({active = active} as game) =
                let newGame =
                    {game with active = {active with rotation = Rotation.Next active.rotation}}
                
                if not <| activeCollides newGame then newGame else game

            let move direction (game: Game) =
                let x,y = game.active.position
                let x,y = match direction with Left -> x-1,y | Right -> x+1,y
                let newGame =
                    {game with active = {game.active with position = x, y}}
                if not <| activeCollides newGame then
                    newGame
                else
                    game

            let step ``type`` game =
                let x,y = game.active.position

                if ``type`` = Auto then
                    stepIn game.speed

                ActiveTile.Map game.active
                |> List.exists (fun (tx,ty) -> game.grid |> Grid.hasSquare (tx,ty+1) || (ty+1) >= game.grid.height)
                |> function
                | true -> game |> (lock >> spawn) |> clearRows
                | false -> {game with active = {game.active with position = (x,y+1)}}  

            let rec drop ({active = active} as game) =
                let x,y = active.position
                let newGame = {game with active = {active with position = x, y+1}}
                if activeCollides newGame then
                    step Manual game
                else
                    drop newGame
            

            let newGame () =
                {
                    active = {tile = pieceGenerator (); position = startPosition; rotation = Zero}
                    hold = None
                    holdLock = false
                    next = pieceGenerator ()
                    grid = Grid.create (width, height)
                    score = 0
                    speed = 1.0
                    over = false
                }                

            let rec handle game =
                async {

                    let! message = mailbox.Receive ()
   
                    match message with
                    | State reply ->
                        reply.Reply game
                        return! handle game
                    | Move direction when not game.over ->
                        return!  move direction game |> handle
                    | Rotate when not game.over ->
                        return! rotate game |> handle
                    | Step ``type`` when not game.over ->
                        return! step ``type`` game |> handle
                    | Drop when not game.over ->
                        return! drop game |> handle
                    | Hold when not game.over ->
                        return! hold game |> handle
                    | _ ->
                        return! handle game
                }
            
            let game = newGame ()

            stepIn game.speed
        
            handle game
        
        ) |> Agent


    
    let state (Agent agent) = agent.PostAndReply State

    let over agent = (state agent).over

    let rotate (Agent agent) = agent.Post Rotate

    let move direction (Agent agent) = agent.Post (Move direction)
    let moveLeft = move Left
    let moveRight = move Right

    let step (Agent agent) = agent.Post (Step Manual)

    let drop (Agent agent) = agent.Post Drop

    let hold (Agent agent) = agent.Post Hold

    let startFrom agent =
        let {grid = grid} = state agent
        start (grid.width, grid.height)


type GameWindow (width, height, game) as this =
    inherit Form ()

    let mutable game = game

    do
        this.Text <- "Ernstris"
        this.ClientSize <- Size(width,height)
        
        this.DoubleBuffered <- true

    let draw  =
        let backgroundBrush = new SolidBrush(Color.FromArgb(0xFF292929))
        let textBrush = new SolidBrush(Color.FloralWhite)
        let transparentBlackBrush = new SolidBrush(Color.FromArgb(0x7F000000))
        let fontName = "Times New Roman"
        let mediumFont = new Font(fontName, 14.0f)
        let smallFont = new Font(fontName,10.0f)
        let largeFont = new Font(fontName,36.0f)
        let squareBrush = new SolidBrush(Color.Red)
        let tileBrush = new SolidBrush(Color.LawnGreen)
        fun (screen: Graphics) (width,height) game ->

            let grid = game.grid

            let squareSize = width / grid.width

            let drawBackground () =
                screen.FillRectangle(backgroundBrush,Rectangle(0,0,width,height))

            let drawSquare brush (x,y) square =
                if y = 0 || y = 1 then
                    () // The (two) top rows are hidden
                else
                    screen.FillRectangle(brush,Rectangle(x*squareSize,y*squareSize,squareSize,squareSize))

            let drawInfo () =

                let drawMiniature sx sy tile =
                    Tile.project tile Rotation.Zero
                    |> List.iter (fun (x,y) ->
                        let size = 7
                        screen.FillRectangle(tileBrush,Rectangle(sx+x*size,sy+y*size,size,size))
                    )                    

                screen.DrawString("ernstris",mediumFont,textBrush,10.0f,10.0f)
                screen.DrawString(sprintf "score: %d" game.score,smallFont,textBrush,10.0f,40.0f)
                screen.DrawString("next:",smallFont,textBrush,(float32 width)-100.0f,10.0f)
                drawMiniature (width-50) 12 game.next
                screen.DrawString("hold:",smallFont,textBrush,(float32 width)-100.0f,40.0f)
                game.hold |> Option.iter(drawMiniature (width-50) 42)              
    
            let drawGameOver () =

                screen.FillRectangle(transparentBlackBrush,this.ClientRectangle)
                let drawFormat = new StringFormat()
                drawFormat.Alignment <- StringAlignment.Center
                screen.DrawString("game over.",largeFont,textBrush,RectangleF(0.0f,(float32 height)/2.0f-100.0f,float32 width,300.0f),drawFormat)
                screen.DrawString("[enter] to play again",mediumFont,textBrush,RectangleF(0.0f,(float32 height)/2.0f,float32 width,300.0f),drawFormat)
                

            do drawBackground ()
            do grid.squares |> Map.iter (drawSquare squareBrush)
            do
                ActiveTile.Map game.active
                |> List.iter (fun position -> drawSquare tileBrush position Square)
            do drawInfo ()
            if game.over then do drawGameOver ()
   
    override this.OnPaint args =
        Game.state game |> draw args.Graphics (width,height)
        async {
            do! Async.Sleep (1000 / 60)
            this.BeginInvoke(new Action(fun _ -> this.Invalidate())) |> ignore
        } |> Async.Start        

    override this.OnKeyDown args =
        match args.KeyCode with
        | Keys.Space ->
            game |> Game.rotate
        | Keys.Left ->
            game |> Game.moveLeft
        | Keys.Right ->
            game |> Game.moveRight
        | Keys.Down ->
            game |> Game.step
        | Keys.Up ->
            game |> Game.drop
        | Keys.Enter ->
            match Game.over game with
            | true -> game <- Game.startFrom game
            | false -> game |> Game.hold
        | _ ->
           ()

 
[<System.STAThread>]
[<EntryPoint>]
let main argv = 

    let width, height = 10,22
    let squareSize = 30
    
    let game = Game.start (width, height)

    use window = new GameWindow(width*squareSize,height*squareSize,game)

    Application.Run(window)

    0 
