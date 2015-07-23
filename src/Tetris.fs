open System
open System.Windows.Forms
open System.Drawing
open System.Threading

// Piece rotation
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


type Square = Square of Image

// Standard tetrominos
type Tile = S | Z | L | J | T | O | I
   
// Functions for piece type
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]        
module Tile =
    
    let rotateImage rotation (image: Image) =
        let rotated = new Bitmap(image.Width,image.Height)
        rotated.SetResolution(image.HorizontalResolution,image.VerticalResolution)
        let g = Graphics.FromImage(rotated)
        g.TranslateTransform((float32 image.Width)/2.0f,(float32 image.Height)/2.0f)
        match rotation with
        | Zero -> ()
        | Ninety -> g.RotateTransform(90.0f)
        | OneEighty -> g.RotateTransform(180.0f)
        | TwoSeventy -> g.RotateTransform(270.0f)
        g.TranslateTransform(-(float32 image.Width)/2.0f,-(float32 image.Height)/2.0f)
        g.DrawImage(image,PointF(0.0f,0.0f))
        rotated :> Image    

    let all = [S; Z; L; J; T; O; I]

    // All squares with all rotations (as a map)
    let squares = 
        let rotations (images : Image []) =
                [(Zero, images)
                 (Ninety, images |> Array.map (rotateImage Ninety))
                 (OneEighty, images |> Array.map (rotateImage OneEighty))
                 (TwoSeventy, images |> Array.map (rotateImage TwoSeventy))]

        all
        |> List.map (fun tile ->
            // Map to resource files
            match tile with
            | T -> Resource.t
            | O -> Resource.o
            | L -> Resource.l
            | I -> Resource.i
            | J -> Resource.j
            | Z -> Resource.z
            | S -> Resource.s
            |> rotations
            |> List.map (fun (rotation,images) -> (tile,rotation), images)
            |> List.toSeq
        )
        |> List.toSeq
        |> Seq.concat
        |> Map.ofSeq
        
    // Get the square associatated with a tile, index in tile (0-3) and rotation 
    let square tile n rotation=
        squares
        |> Map.find (tile, rotation)
        |> (fun array -> array.[n])
        |> Square

    // Get relative tile coordinates from rotation
    let project tile rotation =
        // Table built from: http://tetris.wikia.com/wiki/SRS
        match tile with
        | O ->
            match rotation with
            | Zero ->       [1,0; 2,0; 1,1; 2,1]
            | Ninety ->     [2,0; 2,1; 1,0; 1,1]
            | OneEighty ->  [2,1; 1,1; 2,0; 1,0]
            | TwoSeventy -> [1,1; 1,0; 2,1; 2,0]
        | J ->
            match rotation with
            | Zero ->       [0,0; 0,1; 1,1; 2,1]
            | Ninety ->     [2,0; 1,0; 1,1; 1,2]
            | OneEighty ->  [2,2; 2,1; 1,1; 0,1]
            | TwoSeventy -> [0,2; 1,2; 1,1; 1,0]
        | L ->
            match rotation with
            | Zero ->       [2,0; 0,1; 1,1; 2,1]
            | Ninety ->     [2,2; 1,0; 1,1; 1,2]
            | OneEighty ->  [0,2; 2,1; 1,1; 0,1]
            | TwoSeventy -> [0,0; 1,2; 1,1; 1,0]
        | I ->
            match rotation with
            | Zero ->       [2,0; 2,1; 2,2; 2,3]
            | Ninety ->     [3,2; 2,2; 1,2; 0,2]
            | OneEighty ->  [1,3; 1,2; 1,1; 1,0]
            | TwoSeventy -> [0,1; 1,1; 2,1; 3,1]
        | S ->
            match rotation with
            | Zero ->       [1,0; 2,0; 0,1; 1,1]
            | Ninety ->     [2,1; 2,2; 1,0; 1,1]
            | OneEighty ->  [1,2; 0,2; 2,1; 1,1]
            | TwoSeventy -> [0,1; 0,0; 1,2; 1,1]
        | Z ->
            match rotation with
            | Zero ->       [0,0; 1,0; 1,1; 2,1]
            | Ninety ->     [2,0; 2,1; 1,1; 1,2]
            | OneEighty ->  [2,2; 1,2; 1,1; 0,1]
            | TwoSeventy -> [0,2; 0,1; 1,1; 1,0]
        | T ->
            match rotation with
            | Zero ->       [1,0; 0,1; 2,1; 1,1]
            | Ninety ->     [2,1; 1,0; 1,2; 1,1]
            | OneEighty ->  [1,2; 2,1; 0,1; 1,1]
            | TwoSeventy -> [0,1; 1,2; 1,0; 1,1]       

// The tile that the player is manipulating
type ActiveTile =
    {
        tile: Tile
        position: int*int
        rotation: Rotation
    } with
        // Translate relative tile coordinates to world coordinate susing position and rotation
        static member map {tile = tile; position = (x,y); rotation = rotation} =
            Tile.project tile rotation |> List.map(fun (tx,ty) -> tx+x,ty+y)
        static member move (dx,dy) ({position = (x,y)} as active) = {active with position = x+dx,y+dy}

type Grid = { squares: Map<int*int,Square>
              width: int
              height: int }

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
    
    // Returns a random piece every time it's called (very functional, I know)
    let pieceGenerator =

        let random = Random()

        // Knuth shuffle
        let shuffle (list: 'a list) =
            let array = Array.ofList list // Arrays are ugly, use lists for interface             
            let Swap i j =                                                 
                let item = array.[i]
                array.[i] <- array.[j]
                array.[j] <- item
            let length = array.Length
            [0..(length - 2)]                                                   
            |> Seq.iter (fun i -> Swap i (random.Next(i, length)))                 
            array |> Array.toList // Back to list                                                            

        // Agent
        MailboxProcessor<AsyncReplyChannel<Tile>>.Start(fun mailbox ->
            let rec generate bag = async {

                match bag with
                | head::tail ->
                    let! message = mailbox.Receive ()
                    message.Reply head
                    return! generate tail
                | [] ->
                    // Generate a new "bag"
                    return! Tile.all |> shuffle |> generate
            }

            generate []
        ) 
        |> (fun mailbox () -> mailbox.PostAndReply id)

    type Reply<'a> = AsyncReplyChannel<'a>

    type Direction = Left | Right

    type StepType = Manual | Auto

    type Message = Step of StepType | State of Reply<Game> | Rotate | Move of Direction | Drop | Hold

    type Agent = Agent of MailboxProcessor<Message>

    let start (width, height) =
        MailboxProcessor.Start(fun mailbox ->
            
            // Where to spawn new pieces
            let startPosition = width/2 - 2, 0

            let stepIn speed =
                async {
                    do! Async.Sleep(750.0 / speed |> int)
                    mailbox.Post (Step Auto)
                } |> Async.Start

            let moveActive d ({active = active} as game) =
                {game with active = ActiveTile.move d active}

            // Lock active tile into grid
            let lock (game: Game) =
                let grid, _ =
                    ActiveTile.map game.active
                    |> List.fold (fun (grid,i) position ->
                                    grid
                                    |> Grid.addSquare (Tile.square game.active.tile i game.active.rotation) position, i+1)
                        (game.grid,0)
                {game with grid = grid}
                |> (fun game ->
                    if game.grid.squares |> Map.exists (fun (x,y) _ -> y <= 1) then
                        {game with over = true} // Like poetry
                    else
                        game
                )

            // Is there anything (square or wall) at position?
            let collides ({active = active; grid = grid} as game) (x,y) =
                if x < 0 || x >= grid.width then true
                elif y >= grid.height then true
                elif grid |> Grid.hasSquare (x,y) then true
                else false

            // Is the active piece colliding?
            let activeCollides ({active = active} as game) =
                ActiveTile.map active |> List.exists (collides game)
                
            // Clear any full rows
            let clearRows ({grid = grid} as game) =
                
                let columns = {0..(grid.width-1)} |> List.ofSeq
                let rows = {0..(grid.height-1)}  |> List.ofSeq

                let isRowFilled grid y =
                    columns |> List.exists (fun x -> grid |> Grid.hasSquare (x,y) |> not) |> not
                
                // Recursively clear full rows from top to bottom
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
                            { game with
                                speed = game.speed + 0.05 // Maybe increase exponentially?
                                score = game.score + (game.speed * 300.0 |> int) // TODO: Award bonuses for multiple rows, combos
                            }
                        )
                        |> clearRow row // The removed may now be filled again (or?), anyways recurse on same row.

                    else
                        clearRow (row + 1) game
                                               
                // Start recursion from the top
                clearRow 0 game

            // Spawn a new active piece
            let spawn game =
                {game with
                    active = {tile = game.next; position = startPosition; rotation = Zero}
                    holdLock = false
                    next = pieceGenerator ()}
            // Switch the active piece with the one in hold (or spawn if empty)
            let hold (game: Game) =
                match game.hold with
                | Some tile when not game.holdLock ->
                    {game with
                        active = {tile = tile; position = startPosition; rotation = Rotation.Zero}
                        hold = Some game.active.tile
                        holdLock = true}
                | None when not game.holdLock ->
                    {spawn game with
                        hold = Some game.active.tile
                        holdLock = true}
                | _ -> game

            // Rotate piece (if possible)
            let rotate ({active = active} as game) =
                let newGame =
                    {game with
                        active = {active with rotation = Rotation.Next active.rotation}}
                
                if not <| activeCollides newGame then newGame else game

            // Try moving the active piece.
            let move direction game =
                let dx = match direction with Left -> -1 | Right -> 1
                let newGame = game |> moveActive (dx,0)
                if not <| activeCollides newGame then
                    newGame
                else
                    game

            // Step the active piece one step down (todo: implement lock delay)
            let step ``type`` game =
                let x,y = game.active.position

                if ``type`` = Auto then
                    stepIn game.speed

                ActiveTile.map game.active
                |> List.exists (fun (tx,ty) -> game.grid |> Grid.hasSquare (tx,ty+1) || (ty+1) >= game.grid.height)
                |> function
                | true -> game |> lock |> spawn |> clearRows
                | false -> game |> moveActive (0,1)

            // Instantly drop the active piece into lock.
            let rec drop game =
                let newGame = game |> moveActive (0,1)
                if activeCollides newGame then
                    step Manual game
                else
                    drop newGame
            
            // Create a new game 
            let newGame () =
                {
                    active = { tile = pieceGenerator ()
                               position = startPosition
                               rotation = Zero }
                    hold = None
                    holdLock = false
                    next = pieceGenerator ()
                    grid = Grid.create (width, height)
                    score = 0
                    speed = 1.0
                    over = false
                }                

            // Dispatch game logic and handle events
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
                    | _ -> // When a guard above didn't match
                        return! handle game
                }
            
            let game = newGame ()

            stepIn game.speed
        
            handle game
        
        ) |> Agent

    // Client interface ->
    
    let state (Agent agent) = agent.PostAndReply State

    let over agent = (state agent).over

    let rotate (Agent agent) = agent.Post Rotate

    let move direction (Agent agent) = agent.Post (Move direction)
    let moveLeft = move Left
    let moveRight = move Right

    let step (Agent agent) = agent.Post (Step Manual)

    let drop (Agent agent) = agent.Post Drop

    let hold (Agent agent) = agent.Post Hold

    // Start a new game with the same settings as a previous game
    let startFrom agent =
        let {grid = grid} = state agent
        start (grid.width, grid.height)


type GameWindow (width, height, game) as this =
    inherit Form ()

    // Sorry for mutable field, but this is a simple way to have multiple games without
    // spawning a new window every time.
    let mutable game = game

    do
        this.Text <- Resource.title
        this.ClientSize <- Size(width,height)
        
        // Avoid flickering!!
        this.DoubleBuffered <- true

    let draw  =
        // Todo make these soft (put in resources)
        let backgroundBrush = new SolidBrush(Color.FromArgb(0xFF292929)) // Dark grey
        let textBrush = new SolidBrush(Color.FloralWhite)
        let transparentBlackBrush = new SolidBrush(Color.FromArgb(0x7F000000))
        let fontName = "Times New Roman"
        let mediumFont = new Font(fontName, 14.0f)
        let smallFont = new Font(fontName,10.0f)
        let largeFont = new Font(fontName,36.0f)
        let squareBrush = new SolidBrush(Color.Red)
        let tileBrush = new SolidBrush(Color.LawnGreen)
        let outlinePen = new Pen(Color.Black)
        outlinePen.Width <- 2.0f
        fun (screen: Graphics) (width,height) game ->

            let grid = game.grid

            let squareSize = width / grid.width

            let drawBackground () =
                screen.FillRectangle(backgroundBrush,Rectangle(0,0,width,height))

            let drawSquare (x,y) (Square image) =
                if y = 0 || y = 1 then
                    () // The (two) top rows are hidden
                else
                    let rect = Rectangle(x*squareSize,y*squareSize,squareSize,squareSize)
                    screen.DrawImage(image,rect)
                    screen.DrawRectangle(outlinePen,rect)

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
            do grid.squares |> Map.iter (drawSquare)
            do
                ActiveTile.map game.active
                |> List.iteri (fun i position -> drawSquare position (Tile.square game.active.tile i game.active.rotation))
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
    let squareSize = 40 // <- this will determine window size.
    
    let game = Game.start (width, height)

    use window = new GameWindow(width*squareSize,height*squareSize,game)

    Application.Run(window)

    0 
