// Learn more about F# at http://fsharp.org

open System

type Order =
    | North
    | South
    | East
    | West
    | Start 
    | CutEngine

type MineFieldTerrain =
    | Wall 
    | Field
    | Mine
    | Robot

type minefield = MineFieldTerrain List List

type position = int*int

let CharToOptionMineFieldTerrain = function 
    | '+' ->  Some Wall
    | '0' ->  Some Field
    | '*' ->  Some Mine
    | 'M' ->  Some Robot
    |  _  ->  None

let charToOptionOrder = function 
    | 'N' -> Some North
    | 'S' -> Some South
    | 'E' -> Some East
    | 'O' -> Some West
    | 'I' -> Some Start
    | '-' -> Some CutEngine
    | _   -> None

let mineFieldToChar = function
    | Wall  -> '+'
    | Field -> '0'
    | Mine  -> '*' 
    | Robot -> 'M'

let addTuple (a:position) (b:position) =
    ((fst a + fst b),(snd a + snd b))

let replaceListValue i value lis =
    lis |> List.mapi(fun j x -> if i = j then value else x)

let printGrid (mfield:minefield) =
    mfield |> List.fold( (fun acc x -> acc +  (x |> List.fold(fun acc2 y -> acc2 + (mineFieldToChar y |> Char.ToString) ) "") + Environment.NewLine )) "" 
    |> printfn "%s" 

let redrawField (mField:minefield) (currentPosition:position)  (nextPosition:position) = 
    mField |> List.mapi(fun i x -> match i with
                                   | v when v = (fst currentPosition) -> replaceListValue (snd currentPosition)  Field x   
                                   | _ -> x 
                                   ) |> List.mapi(fun i x -> match i with                                             
                                                             | v when v = (fst nextPosition) ->  replaceListValue (snd nextPosition) Robot x
                                                             | _ -> x )|>
    
    printGrid 


let findStartPoistion (mfield:minefield) = 
    let first = mfield 
                |> List.findIndex(fun x -> x |> List.exists(fun y-> y = Robot))

    let second = mfield |> List.item first 
                        |> List.findIndex(fun y-> y = Robot)
    (first,second)

let clearMineField (mField:minefield) =  
    mField 
    |> List.map(fun x -> x |> List.map(fun y -> if y = Robot then Field else y))

let executeMoves  (mfield:minefield) (moves:position list) = 
    let startposition = mfield 
                       |> findStartPoistion 
    let newField = mfield 
                   |> clearMineField

    let rec proccessMove (mField:minefield) (moves:position list) (currentPosition:position) = 
        match (moves |> List.tryHead) with 
        | None -> match (snd currentPosition) = ((mField |> List.item (fst currentPosition) |> List.length) - 1) with
                  | true ->  printfn "Escaped" 
                  | false -> printfn "Alive but lost"
        | Some moveDirection -> let nextPosition = addTuple currentPosition moveDirection
                                match mField |> List.item (fst nextPosition) |> List.item (snd nextPosition) with 
                                | Wall ->  proccessMove mField (moves |> List.tail) currentPosition
                                | Field -> redrawField mField currentPosition nextPosition 
                                           proccessMove mField (moves |> List.tail) nextPosition
                                | Mine ->  printfn "RIP bot" 
                                | _ ->     proccessMove mField (moves |> List.tail) currentPosition

    proccessMove (newField:minefield) (moves:position list) startposition

    
let createMoves (orders:Order list) =

    let rec translateOrders (orders:Order list) (moves:position list) engine =
        match orders |> List.tryHead with 
        | None -> moves |> List.rev
        | Some currentOrder ->
               match currentOrder with
               | North when engine -> translateOrders (orders |> List.tail) ((-1,0)::moves) engine
               | South when engine -> translateOrders (orders |> List.tail) ((1,0)::moves) engine
               | East  when engine -> translateOrders (orders |> List.tail) ((0,1)::moves) engine
               | West  when engine -> translateOrders (orders |> List.tail) ((0,-1)::moves) engine
               | Start when not engine -> translateOrders (orders |> List.tail) moves true
               | CutEngine when engine -> translateOrders (orders |> List.tail) moves false
               | _ -> translateOrders (orders |> List.tail) moves engine

    translateOrders orders [] false


///Reads all the valid orders

let readOrders s = 
   s  |> Seq.choose( fun x -> charToOptionOrder x) 
      |> Seq.toList 

  
let createMinefield () =

    let rec readeMineField (reader:unit ->string) (field:minefield) = 
        match reader() with 
        | null  -> field 
                   |> List.rev // Since the recusrion makes the list appear out of order

        | nchar -> readeMineField reader ((nchar   
                   |> Seq.choose(fun x-> (CharToOptionMineFieldTerrain x)) |> Seq.toList)  :: field)

    readeMineField Console.ReadLine [] 


[<EntryPoint>]
let main argv =
    printfn "Enter your minefield"
    let mine = (createMinefield())
    printfn "Enter your Orders"
    (readOrders (Console.ReadLine()))  
    |> createMoves  
    |> executeMoves mine 
    0 // return an integer exit code
