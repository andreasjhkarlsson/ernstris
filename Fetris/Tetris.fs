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
    next: Tile
    grid: Grid
    score: int
    speed: float
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

    type Message = Step of StepType | State of Reply<Game> | Rotate | Move of Direction | Drop


    let start (width, height) =
        MailboxProcessor.Start(fun mailbox ->

            let stepIn speed =
                async {
                    do! Async.Sleep(500.0 / speed |> int)
                    mailbox.Post (Step Auto)
                } |> Async.Start

            let lock (game: Game) =
                let grid =
                    ActiveTile.Map game.active
                    |> List.fold (fun grid position -> grid |> Grid.addSquare Square position) game.grid
                {game with grid = grid}

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
                        |> clearRow row

                    else
                        clearRow (row + 1) game
                                               

                clearRow 0 game

            let spawn game =
                {game with
                    active = {tile = game.next; position = (2,0); rotation = Zero}
                    next = pieceGenerator ()}

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
                    
                

            let rec handle game =
                async {
                    let! message = mailbox.Receive ()
                    match message with
                    | State reply ->
                        reply.Reply game
                        return! handle game
                    | Move direction ->
                        return!  move direction game |> handle
                    | Rotate ->
                        return! rotate game |> handle
                    | Step ``type`` ->
                        return! step ``type`` game |> handle
                    | Drop ->
                        return! drop game |> handle
                }
            
            stepIn 1.0
        
            handle {
                active = {tile = pieceGenerator (); position = 2,0; rotation = Zero}
                hold = None
                next = pieceGenerator ()
                grid = Grid.create (width, height)
                score = 0
                speed = 1.0
            }
        
        ) |> Agent

    
    let state (Agent agent) = agent.PostAndReply State

    let rotate (Agent agent) = agent.Post Rotate

    let move direction (Agent agent) = agent.Post (Move direction)
    let moveLeft = move Left
    let moveRight = move Right

    let step (Agent agent) = agent.Post (Step Manual)

    let drop (Agent agent) = agent.Post Drop

type GameWindow (width, height, game) as this =
    inherit Form ()


    do
        this.Text <- "Ernstris"
        this.ClientSize <- Size(width,height)
        
        this.DoubleBuffered <- true

    let draw  =
        let backgroundBrush = new SolidBrush(Color.FromArgb(0xFF292929))
        let gridPen = new Pen(Color.FloralWhite)
        let squareBrush = new SolidBrush(Color.Red)
        let tileBrush = new SolidBrush(Color.LawnGreen)
        fun (screen: Graphics) (width,height) game ->

            let grid = game.grid

            let squareSize = width / grid.width

            let drawBackground () =
                screen.FillRectangle(backgroundBrush,Rectangle(0,0,width,height))

            let drawSquare brush (x,y) square =
                screen.FillRectangle(brush,Rectangle(x*squareSize,y*squareSize,squareSize,squareSize))
    
            do drawBackground ()
            do grid.squares |> Map.iter (drawSquare squareBrush)
            Tile.project game.active.tile game.active.rotation
            |> List.map (fun (x,y) -> x + (fst game.active.position), y + (snd game.active.position))
            |> List.iter (fun position -> drawSquare tileBrush position Square)
   
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
