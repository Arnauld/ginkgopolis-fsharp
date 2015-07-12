﻿module ginkgopolis.core

open Microsoft.FSharp.Reflection

// -------------------------------------------------------------------------------
// @see Railway programming http://fsharpforfunandprofit.com/rop/
type TwoTrack<'TEntity, 'TError> = 
    | Success of 'TEntity
    | Error of 'TError

// -------------------------------------------------------------------------------
type ResourceColor = 
    | Blue
    | White
    | Orange
    | LightBrown
    | Green

type SuccessPoint = unit

//             _                 _          _   _             
//  /\ /\ _ __| |__   __ _ _ __ (_)______ _| |_(_) ___  _ __  
// / / \ \ '__| '_ \ / _` | '_ \| |_  / _` | __| |/ _ \| '_ \ 
// \ \_/ / |  | |_) | (_| | | | | |/ / (_| | |_| | (_) | | | |
//  \___/|_|  |_.__/ \__,_|_| |_|_/___\__,_|\__|_|\___/|_| |_|

type UrbanizationToken = 
    | A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L

let AllUrbanizationTokens : UrbanizationToken list = [ A; B; C; D; E; F; G; H; I; J; K; L ]

type UrbanizationCardId = UrbanizationToken

let AllUrbanizationCardIds : UrbanizationCardId list = AllUrbanizationTokens

//    ___       _ _     _ _               _____ _ _      
//   / __\_   _(_) | __| (_)_ __   __ _  /__   (_) | ___ 
//  /__\// | | | | |/ _` | | '_ \ / _` |   / /\/ | |/ _ \
// / \/  \ |_| | | | (_| | | | | | (_| |  / /  | | |  __/
// \_____/\__,_|_|_|\__,_|_|_| |_|\__, |  \/   |_|_|\___|
//                                |___/                  

type BuildingColor = 
    | Blue
    | Red
    | Yellow

type BuildingNum = int

type BuildingTile = 
    { color : BuildingColor
      number : BuildingNum }
    override m.ToString() = (sprintf "BuildingTile {%A, %A}" m.color m.number)

let newBuildingTile (color : BuildingColor) (number : BuildingNum) = 
    { color = color
      number = number }

let generateBuildingTiles (min, max) : BuildingTile list = 
    [ for num in min..max do
          for color in [ Blue; Red; Yellow ] -> (newBuildingTile color num) ]

let initialBuildingTiles = generateBuildingTiles (1, 3)


type GreenSpaceTile = unit

type Tile = 
    | BuildingTile of BuildingTile
    | GreenSpaceTile of GreenSpaceTile

let asTile (buildingTile:BuildingTile) = BuildingTile buildingTile


//    ___              _ 
//   / __\__ _ _ __ __| |
//  / /  / _` | '__/ _` |
// / /__| (_| | | | (_| |
// \____/\__,_|_|  \__,_|
                      

type BuildingCardId = BuildingTile

type CardId = 
    | UrbanizationCardId of UrbanizationCardId
    | BuildingCardId of BuildingCardId

//    ___ _                          _            
//   / __\ |__   __ _ _ __ __ _  ___| |_ ___ _ __ 
//  / /  | '_ \ / _` | '__/ _` |/ __| __/ _ \ '__|
// / /___| | | | (_| | | | (_| | (__| ||  __/ |   
// \____/|_| |_|\__,_|_|  \__,_|\___|\__\___|_|   
                                               

type CharacterId = int

type CharacterGroup = 
    | NoGroup
    | Group1
    | Group2
    | Group3
    | Group4
    | Group5
    | Group6
    | Expert

type InitialItem = 
    | Resource = 0
    | SuccessPoint = 1
    | BuildingTile = 2
    | Event = 3

//    __                 _   
//   /__\_   _____ _ __ | |_ 
//  /_\ \ \ / / _ \ '_ \| __|
// //__  \ V /  __/ | | | |_ 
// \__/   \_/ \___|_| |_|\__|
                          
type Event = 
    | DesignCompetition
    | AdvertisingCampaign
    | Expropriation
    | QualityControl


//    ___ _                       
//   / _ \ | __ _ _   _  ___ _ __ 
//  / /_)/ |/ _` | | | |/ _ \ '__|
// / ___/| | (_| | |_| |  __/ |   
// \/    |_|\__,_|\__, |\___|_|   
//                |___/           

type Player = string

type PlayerId = 
    | Player1
    | Player2
    | Player3
    | Player4
    | Player5

let AllPlayerIds = [ Player1; Player2; Player3; Player4; Player5 ]

type PlayerState = 
    { nbResource : int
      nbResourceAvailable : int
      nbSuccessPoint : int
      nbTilePoint: int
      cards : BuildingCardId list
      characters : CharacterId list
      tiles : Tile list }

type PlayerHand = CardId list


//    ___                     
//   / _ \__ _ _ __ ___   ___ 
//  / /_\/ _` | '_ ` _ \ / _ \
// / /_\\ (_| | | | | | |  __/
// \____/\__,_|_| |_| |_|\___|
                           
type Game = 
    { playersToIds : Map<Player, PlayerId>
      playerStates : Map<PlayerId, PlayerState>
      playerHands : Map<PlayerId, PlayerHand>
      availableTiles : Tile list }

type GameError = 
    | TooMuchPlayer of Player list
    | UnknownPlayer of Player
    | PlayerIdNotBound of PlayerId
    | NoResourceAvailable
    | NoTileAvailableInGame
    | UnsupportedGain

let playerStateOf (player : PlayerId) (gameTT : TwoTrack<Game, GameError>) : TwoTrack<PlayerState, GameError> = 
    match gameTT with
    | Error e -> Error e
    | Success game -> 
        let ps = game.playerStates
        let ts = game.availableTiles
        match ps.TryFind(player) with
        | None -> Error(PlayerIdNotBound player)
        | Some playerState -> Success playerState

let withinPlayerStateOf<'T> (player : PlayerId) (gameTT : TwoTrack<Game, GameError>) (func : PlayerState -> 'T) : TwoTrack<'T, GameError> = 
    let playerStateTT = playerStateOf player gameTT
    match playerStateTT with
    | Error e -> Error e
    | Success ps -> Success(func ps)

// interleave player and playerIds
let rec mapPlayers (players : Player list) (playerIds : PlayerId list) (mapped : Map<Player, PlayerId>) = 
    match playerIds with
    | id :: ids -> 
        match players with
        | p :: ps -> mapPlayers ps ids (mapped.Add(p, id))
        | [] -> Success mapped
    | [] -> Error(TooMuchPlayer players)


// --
// http://www.devx.com/dotnet/Article/40537/0/page/3#sthash.xM1pn761.dpuf
let randomNumberGenerator = new System.Random()

// type is specified to prevent crash on mono...
let shuffle (cards : Tile list) = 
    let upperBound = (List.length cards) * 100
    let weightedCards = List.map (fun card -> card, randomNumberGenerator.Next(0, upperBound)) cards
    let sortedWeightedCards = 
        List.sortWith (fun (_, leftWeight) (_, rightWeight) -> leftWeight - rightWeight) weightedCards
    List.map (fun (card, _) -> card) sortedWeightedCards


let newGame (players : Player list) (availableTiles : Tile list) : TwoTrack<Game, GameError> = 
    match mapPlayers players AllPlayerIds Map.empty with
    | Error err -> Error err
    | Success ids -> 
        let initialPlayerState = 
            { nbResource = 0
              nbResourceAvailable = 0
              nbSuccessPoint = 0
              nbTilePoint = 0
              cards = []
              characters = []
              tiles = [] }
        
        let playerStates = 
            ids
            |> Map.toList
            |> List.map (fun (p, id) -> (id, initialPlayerState))
            |> Map.ofList
        
        Success { playersToIds = ids
                  playerStates = playerStates
                  playerHands = Map.empty
                  availableTiles = availableTiles }

//    _        _   _             
//   /_\   ___| |_(_) ___  _ __  
//  //_\\ / __| __| |/ _ \| '_ \ 
// /  _  \ (__| |_| | (_) | | | |
// \_/ \_/\___|\__|_|\___/|_| |_|
                              

type ActionKind = 
    | Exploit = 1
    | Urbanization = 2
    | FloorConstruction = 3
    | EndGame = 10

type UpdateGame = PlayerId -> TwoTrack<Game, GameError> -> TwoTrack<Game, GameError>

let identityUpdateGame : UpdateGame = fun (player : PlayerId) (gameTT : TwoTrack<Game, GameError>) -> gameTT

type OnAction = ActionKind -> UpdateGame

type Gain = 
    | Resource = 0
    | AvailableResource = 1
    | SuccessPoint = 2
    | Tile = 3

let gain (item : Gain) (player : PlayerId) (gameTT : TwoTrack<Game, GameError>) = 
    match gameTT with
    | Error _ -> gameTT
    | Success game -> 
        let ps = game.playerStates
        let ts = game.availableTiles
        match ps.TryFind(player) with
        | None -> Error(PlayerIdNotBound player)
        | Some playerState -> 
            let playerState = ps.[player]
            match item with
            | Gain.Resource -> 
                if playerState.nbResourceAvailable > 0 then 
                    let newState = 
                        { playerState with nbResource = playerState.nbResource + 1
                                           nbResourceAvailable = playerState.nbResourceAvailable - 1 }
                    Success { game with playerStates = ps.Add(player, newState) }
                else Error NoResourceAvailable
            | Gain.AvailableResource -> 
                let newState = { playerState with nbResourceAvailable = playerState.nbResourceAvailable + 1 }
                Success { game with playerStates = ps.Add(player, newState) }
            | Gain.SuccessPoint -> 
                let newState = { playerState with nbSuccessPoint = playerState.nbSuccessPoint + 1 }
                Success { game with playerStates = ps.Add(player, newState) }
            | Gain.Tile -> 
                let newState = { playerState with nbTilePoint = playerState.nbTilePoint + 1 }
                Success { game with playerStates = ps.Add(player, newState) }
            | _ -> Error UnsupportedGain

let consumeTilePoint () = 
//                match ts with
//                | [] -> Error NoTileAvailableInGame
//                | tile :: remainings -> 
//                    let newState = { playerState with nbTilePoint = nbTilePoint + 1 }
//                    Success { game with playerStates = ps.Add(player, newState)
//                                        availableTiles = remainings }
   failwith "Not implemented"


let whenAction (requiredKind : ActionKind) (updater : UpdateGame) = 
    fun (kind : ActionKind) -> 
        if kind = requiredKind then updater
        else identityUpdateGame

let NoAction:OnAction = fun (kind : ActionKind) -> identityUpdateGame 

//    ___ _                          _                _        _   _             
//   / __\ |__   __ _ _ __ __ _  ___| |_ ___ _ __    /_\   ___| |_(_) ___  _ __  
//  / /  | '_ \ / _` | '__/ _` |/ __| __/ _ \ '__|  //_\\ / __| __| |/ _ \| '_ \ 
// / /___| | | | (_| | | | (_| | (__| ||  __/ |    /  _  \ (__| |_| | (_) | | | |
// \____/|_| |_|\__,_|_|  \__,_|\___|\__\___|_|    \_/ \_/\___|\__|_|\___/|_| |_|
                                                                              
type CharacterCard = 
    { id : CharacterId
      color : BuildingColor
      group : CharacterGroup
      initialItems : InitialItem list
      onAction : OnAction }

type CharacterCards = CharacterId -> CharacterCard

let character1 : CharacterCard = 
    { id = 1
      color = Red
      group = Group1
      initialItems = [ InitialItem.Resource; InitialItem.SuccessPoint; InitialItem.BuildingTile ]
      onAction = whenAction ActionKind.Urbanization (gain Gain.Resource) }

//    ___       _ _     _ _                 _        _   _             
//   / __\_   _(_) | __| (_)_ __   __ _    /_\   ___| |_(_) ___  _ __  
//  /__\// | | | | |/ _` | | '_ \ / _` |  //_\\ / __| __| |/ _ \| '_ \ 
// / \/  \ |_| | | | (_| | | | | | (_| | /  _  \ (__| |_| | (_) | | | |
// \_____/\__,_|_|_|\__,_|_|_| |_|\__, | \_/ \_/\___|\__|_|\___/|_| |_|
//                                |___/                                

type BuildingActions = BuildingCardId -> OnAction

let defaultBuildingActions:BuildingActions = 
    fun cardId ->
        match (cardId.color, cardId.number) with
        | (Blue, 1) -> whenAction ActionKind.Exploit (gain Gain.Tile)
        | (Blue, 2) -> whenAction ActionKind.Urbanization (gain Gain.Tile)
        | (Blue, 3) -> whenAction ActionKind.FloorConstruction (gain Gain.Tile)
        | (Blue, 4) -> whenAction ActionKind.Exploit (gain Gain.Tile)
        | (Blue, 5) -> whenAction ActionKind.Urbanization (gain Gain.Tile)
        | (Blue, 6) -> whenAction ActionKind.FloorConstruction (gain Gain.Tile)
        | _ -> NoAction

//    __                         _   
//   / /  __ _ _   _  ___  _   _| |_ 
//  / /  / _` | | | |/ _ \| | | | __|
// / /__| (_| | |_| | (_) | |_| | |_ 
// \____/\__,_|\__, |\___/ \__,_|\__|
//             |___/                 

type Coord = int * int

type BuildingBlock = 
    { inConstruction : bool
      tiles : BuildingTile list
      player : Option<Player>
      nbResource : int }

type GreenSpaceBlock = 
    { tile : GreenSpaceTile
      player : Option<Player>
      nbResource : int }

type CityBlock = 
    | UrbanizationBlock of UrbanizationToken
    | BuildingBlock of BuildingBlock
    | GreenSpaceBlock of GreenSpaceBlock

type CityLayout = Map<Coord, CityBlock>

type City = 
    { layout : CityLayout }

let layoutCity (buildingTiles : Tile list) = 
    let urbz = 
        [ ((-1, +2), UrbanizationBlock A)
          ((0, +2), UrbanizationBlock B)
          ((+1, +2), UrbanizationBlock C)
          ((+2, +1), UrbanizationBlock D)
          ((+2, 0), UrbanizationBlock E)
          ((+2, -1), UrbanizationBlock F)
          ((+1, -2), UrbanizationBlock G)
          ((0, -2), UrbanizationBlock H)
          ((-1, -2), UrbanizationBlock I)
          ((-2, -1), UrbanizationBlock J)
          ((-2, 0), UrbanizationBlock K)
          ((-2, +1), UrbanizationBlock L) ]
    
    let coords : Coord list = 
        [ for y in [ 1; 0; -1 ] do
              for x in [ -1; 0; 1 ] -> (x, y) ]
    
    let initBlock (tile : Tile) : CityBlock = 
        match tile with
        | BuildingTile bt -> 
            BuildingBlock { inConstruction = false
                            tiles = [ bt ]
                            player = Option.None
                            nbResource = 0 }
        | GreenSpaceTile gs -> 
            GreenSpaceBlock { tile = gs
                              player = Option.None
                              nbResource = 0 }
    
    let layoutBT = 
        buildingTiles
        |> List.map initBlock
        |> List.zip coords
        |> List.append urbz
        |> Map.ofList
    
    { layout = layoutBT }

//
//         +-------+-------+-------+
//         |       |       |       |           
//         |   A   |   B   |   C   |           
//         |       |       |       |        
// +-------+-------+-------+-------+-------+    
// |       |       |       |       |       |   
// |   L   |       |       |       |   D   |   
// |       |       |       |       |       |
// +-------+-------+-------+-------+-------+    
// |       |       |       |       |       |   
// |   K   |       |   +   |       |   E   |   
// |       |       |       |       |       |
// +-------+-------+-------+-------+-------+    
// |       |       |       |       |       |   
// |   J   |       |       |       |   F   |   
// |       |       |       |       |       |
// +-------+-------+-------+-------+-------+    
//         |       |       |       |           
//         |   I   |   H   |   G   |           
//         |       |       |       |        
//         +-------+-------+-------+    
//
let rangeOf (cityLayout : CityLayout) : Coord * Coord = 
    cityLayout
    |> Map.toList
    |> List.map (fun (k, v) -> k)
    |> List.fold (fun ((xmin, ymin), (xmax, ymax)) (x, y) -> 
           let nxmin = 
               if x < xmin then x
               else xmin
           
           let nxmax = 
               if x > xmax then x
               else xmax
           
           let nymin = 
               if y < ymin then y
               else ymin
           
           let nymax = 
               if y > ymax then y
               else ymax
           
           ((nxmin, nymin), (nxmax, nymax))) ((+1, +1), (-1, -1))

let printCity (city : City) = 
    let layout = city.layout
    let ((xmin, ymin), (xmax, ymax)) = rangeOf layout
    let cellHeight = 4
    
    let formatUrbz token = 
        [ ("       ")
          ("       ")
          (sprintf "  (%2A)  " token)
          ("       ") ]
    
    let formatBuildingTiles (b : BuildingBlock) (bs : BuildingTile list) = 
        let n = bs.Length
        [ (sprintf "%4A" bs.Head.color)
          (sprintf "%s %6A" (if b.inConstruction then "*"
                             else " ") bs.Head.number)
          (sprintf "%4s" (if b.player.IsSome then b.player.Value
                          else ""))
          (sprintf "#%d    " n) ]
    
    let formatGreenSpace (b : GreenSpaceBlock) = 
        [ ("       ")
          ("   GS  ")
          (sprintf " (%2A)  " (if b.player.IsSome then b.player.Value
                               else ""))
          ("       ") ]
    
    let formatCellInfo (block : CityBlock) = 
        match block with
        | BuildingBlock b -> formatBuildingTiles b b.tiles
        | GreenSpaceBlock b -> formatGreenSpace b
        | UrbanizationBlock token -> formatUrbz token
    
    let printCell (r : int) (y : int) (x : int) = 
        let cityBlockOpt : Option<CityBlock> = layout.TryFind(x, y)
        let exists = cityBlockOpt.IsSome
        let existsOrHasRightBlock = exists || layout.ContainsKey(x + 1, y)
        let existsOrHasBottomBlock = exists || layout.ContainsKey(x, y - 1)
        
        let fmt = 
            match r with
            | 4 when existsOrHasBottomBlock -> "_______"
            | _ when exists -> List.nth (formatCellInfo cityBlockOpt.Value) r
            | _ -> "       "
        printf "%7s%s" fmt (if existsOrHasRightBlock then "|"
                            else " ")
    
    let printRow (y : int) = 
        [ 0..cellHeight ] |> List.iter (fun r -> 
                                 [ xmin..xmax ] |> List.iter (printCell r y)
                                 printfn "")
    
    (List.rev [ (ymin - 1)..(ymax + 1) ]) |> List.iter printRow
