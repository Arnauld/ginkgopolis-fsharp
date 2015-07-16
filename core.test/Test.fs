module core.test

open System
open NUnit.Framework
open ginkgopolis.core

[<TestFixture>]
type ``urbanization tokens``() = 
    [<Test>]
    member x.``there should be 12 differents urbanization tokens``() = 
        let tokens = AllUrbanizationTokens
        Assert.AreEqual(12, List.length tokens)
        Assert.AreEqual(12, Set.count (Set.ofList tokens))

[<TestFixture>]
type ``building tiles``() = 
    
    [<Test>]
    member x.``there should be 1 to 3 for blue, yellow, red building tiles in initial layout``() = 
        let eq (bt : BuildingTile) = (fun other -> other = bt)
        let tiles = initialBuildingTiles
        Assert.AreEqual(9, List.length tiles)
        Assert.IsTrue(List.exists (eq (newBuildingTile Blue 1)) tiles)
        Assert.IsTrue(List.exists (eq (newBuildingTile Blue 2)) tiles)
        Assert.IsTrue(List.exists (eq (newBuildingTile Blue 3)) tiles)
        Assert.IsTrue(List.exists (eq (newBuildingTile Red 1)) tiles)
        Assert.IsTrue(List.exists (eq (newBuildingTile Red 2)) tiles)
        Assert.IsTrue(List.exists (eq (newBuildingTile Red 3)) tiles)
        Assert.IsTrue(List.exists (eq (newBuildingTile Yellow 1)) tiles)
        Assert.IsTrue(List.exists (eq (newBuildingTile Yellow 2)) tiles)
        Assert.IsTrue(List.exists (eq (newBuildingTile Yellow 3)) tiles)
    
    [<Test>]
    member x.``Tile cost should be 1 for lvl1 building and Green space``() = 
        Assert.AreEqual(1, tileCostOf BuildingLvl1)
        Assert.AreEqual(1, tileCostOf GreenSpace)
    
    [<Test>]
    member x.``Tile cost should be 2 for lvl2 building``() = Assert.AreEqual(2, tileCostOf BuildingLvl2)
    
    [<Test>]
    member x.``Tile cost should be 3 for Prestige building``() = Assert.AreEqual(3, tileCostOf PrestigeBuilding)
    
    [<Test>]
    member x.``Tile type should be Lvl1 for buildings 1 to 20``() = 
        [ Blue; Yellow; Red ] |> List.iter (fun color -> 
                                     [ 1..20 ] |> List.iter (fun n -> 
                                                      let t = (newBuildingTile color n)
                                                      Assert.AreEqual(BuildingLvl1, t.TileType)))
    
    [<Test>]
    member x.``Tile type should be Lvl2 for buildings 21 to 23 - expert expansion``() = 
        [ Blue; Yellow; Red ] |> List.iter (fun color -> 
                                     [ 21..23 ] |> List.iter (fun n -> 
                                                       let t = (newBuildingTile color n)
                                                       Assert.AreEqual(BuildingLvl2, t.TileType)))
    
    [<Test>]
    member x.``Tile type should be Prestige for buildings 24 and 25 - expert expansion``() = 
        [ Blue; Yellow; Red ] |> List.iter (fun color -> 
                                     [ 24..25 ] |> List.iter (fun n -> 
                                                       let t = (newBuildingTile color n)
                                                       Assert.AreEqual(PrestigeBuilding, t.TileType)))

[<TestFixture>]
type ``affectIdToPlayers function``() = 
    
    [<Test>]
    member x.``should fail when trying to handle more than 5 players ``() = 
        match affectIdToPlayers [ "John"; "Carmen"; "Pacman"; "Flibuste"; "Colin"; "Martin" ] AllPlayerIds Map.empty with
        | Error(TooMuchPlayer ps) -> Assert.AreEqual([ "Martin" ], ps)
        | Error x -> Assert.Fail(sprintf "Wrong error: %A" x)
        | Success _ -> Assert.Fail "More than 5 players should bot be allowed"
    
    [<Test>]
    member x.``should associate player with playerId``() = 
        match affectIdToPlayers [ "Carmen"; "Pacman" ] AllPlayerIds Map.empty with
        | Error x -> Assert.Fail(sprintf "No error expected: %A" x)
        | Success mapped -> 
            let expected = 
                [ ("Carmen", Player1)
                  ("Pacman", Player2) ]
                |> Map.ofList
            Assert.AreEqual(expected, mapped)

[<TestFixture>]
type ``new game``() = 
    
    [<Test>]
    member x.``new game should fail when there are too much players``() = 
        let game = newGame [ "John"; "Carmen"; "Travis"; "Pacman"; "Vlad"; "Mccallum" ] []
        match game with
        | Error(TooMuchPlayer ps) -> Assert.AreEqual([ "Mccallum" ], ps)
        | Error x -> Assert.Fail(sprintf "Wrong error: %A" x)
        | Success _ -> Assert.Fail "More than 5 players should bot be allowed"
    
    [<Test>]
    member x.``new game should map players' names with ids``() = 
        let game = newGame [ "John"; "Carmen" ] []
        match game with
        | Success s -> 
            Assert.AreEqual(Player1, s.playersToIds.["John"])
            Assert.AreEqual(Player2, s.playersToIds.["Carmen"])
        | Error e -> Assert.Fail(sprintf "No error should have occured, got: %A" e)

[<TestFixture>]
type ``triggered action``() = 
    
    [<Test>]
    member x.``world should not be updated when action triggered and callback types do not match``() = 
        let character1OnAction = whenAction ActionKind.Urbanization (gain [Gain.Resource])
        let game = newGame [ "John"; "Carmen" ] []
        Assert.AreEqual(game, ((character1OnAction ActionKind.EndGame) Player1 game))
        Assert.AreEqual(game, ((character1OnAction ActionKind.FloorConstruction) Player1 game))
    
    [<Test>]
    member x.``increase number of resources should fail when none are available``() = 
        let character1OnAction = whenAction ActionKind.Urbanization (gain [Gain.Resource])
        let game = newGame [ "John"; "Carmen" ] []
        let ng = (character1OnAction ActionKind.Urbanization) Player1 game
        match (withinPlayerStateOf Player1 (fun p -> p.nbResource) ng) with
        | Error e -> Assert.AreEqual(NoResourceAvailable, e)
        | Success v -> Assert.Fail(sprintf "An error should have occured, got: %A" v)
    
    [<Test>]
    member x.``increase number of availables resource should increment its count``() = 
        let ng0 = newGame [ "John"; "Carmen" ] []
        let ng1 = gain [Gain.AvailableResource] Player2 ng0
        let ng2 = gain [Gain.AvailableResource] Player2 ng1
        match (withinPlayerStateOf Player2 (fun p -> p.nbResourceAvailable) ng2) with
        | Error e -> Assert.Fail(sprintf "No error should have occured, got: %A" e)
        | Success v -> Assert.AreEqual(2, v)
    
    [<Test>]
    member x.``increase number of resources should consume player's available resource``() = 
        let character1OnAction = whenAction ActionKind.Urbanization (gain [Gain.Resource])
        let ng0 = newGame [ "John"; "Carmen" ] []
        let ng1 = gain [Gain.AvailableResource] Player2 ng0
        let ng2 = (character1OnAction ActionKind.Urbanization) Player2 ng1
        match (withinPlayerStateOf Player2 (fun p -> p.nbResource) ng2) with
        | Error e -> Assert.Fail(sprintf "No error should have occured, got: %A" e)
        | Success v -> Assert.AreEqual(1, v)
    
    [<Test>]
    member x.``increase number of success point should work!``() = 
        let character1OnAction = whenAction ActionKind.Urbanization (gain [Gain.SuccessPoint])
        let ng0 = newGame [ "John"; "Carmen" ] []
        let ng2 = (character1OnAction ActionKind.Urbanization) Player2 ng0
        match (withinPlayerStateOf Player2 (fun p -> p.nbSuccessPoint) ng2) with
        | Error e -> Assert.Fail(sprintf "No error should have occured, got: %A" e)
        | Success v -> Assert.AreEqual(1, v)
    
    [<Test>]
    member x.``increase tile should no alter available tiles in game and player's hand``() = 
        let character2OnAction = whenAction ActionKind.Urbanization (gain [Gain.Tile])
        
        let availableTiles : Tile list = 
            [ (newBuildingTile Blue 7)
              (newBuildingTile Yellow 5)
              (newBuildingTile Red 9) ]
            |> List.map (fun t -> BuildingTile t)
        
        let ng0 = newGame [ "John"; "Carmen" ] availableTiles
        let ng2 = (character2OnAction ActionKind.Urbanization) Player2 ng0
        //
        // Check player's hand
        //
        let ts = withinPlayerStateOf Player2 (fun p -> (p.tiles, p.nbTilePoint)) ng2
        match ts with
        | Error e -> Assert.Fail(sprintf "No error should have occured, got: %A" e)
        | Success v -> 
            let (player2Tiles, player2TilePoints) = v
            Assert.IsTrue(List.isEmpty player2Tiles)
            Assert.AreEqual(1, player2TilePoints)
        //
        // Check game's available tiles
        //
        match ng2 with
        | Error e -> Assert.Fail(sprintf "No error should have occured, got: %A" e)
        | Success g -> 
            let expectedTiles = 
                [ (newBuildingTile Blue 7)
                  (newBuildingTile Yellow 5)
                  (newBuildingTile Red 9) ]
                |> List.map (fun t -> BuildingTile t)
            Assert.AreEqual(expectedTiles, g.availableTiles)

[<TestFixture>]
type ``gain behaviors``() = 

    [<Test>]
    member x.``an error should occurs when trying to gain a resource but none are available``() = 
        let availableTiles : Tile list = 
            [ (newBuildingTile Blue 21)
              (newBuildingTile Yellow 5)
              (newBuildingTile Red 9) ]
            |> List.map (fun t -> BuildingTile t)
        
        let ng1 = 
            newGame [ "John"; "Carmen" ] availableTiles
            |> (gain [Gain.Resource] Player2)

        match ng1 with
        | Error e -> Assert.AreEqual(NoResourceAvailable, e)
        | Success s -> Assert.Fail(sprintf "An error should have occured, got: %A" s)
    

[<TestFixture>]
type ``draw tile``() = 
    
    [<Test>]
    member x.``an error should occur when trying to draw tile and player has not enough tile points - lvl1 building``() = 
        let availableTiles : Tile list = 
            [ (newBuildingTile Blue 7)
              (newBuildingTile Yellow 5)
              (newBuildingTile Red 9) ]
            |> List.map (fun t -> BuildingTile t)
        
        let ng0 = newGame [ "John"; "Carmen" ] availableTiles |> (drawTile Player2 BuildingLvl1)
        match ng0 with
        | Error e -> Assert.AreEqual(NotEnoughTilePoint(1, 0), e)
        | Success s -> Assert.Fail(sprintf "An error should have occured, got: %A" s)
    
    [<Test>]
    member x.``an error should occur when trying to draw tile and player has not enough tile points - lvl2 building``() = 
        let availableTiles : Tile list = 
            [ (newBuildingTile Blue 21)
              (newBuildingTile Yellow 5)
              (newBuildingTile Red 9) ]
            |> List.map (fun t -> BuildingTile t)
        
        let ng0 = newGame [ "John"; "Carmen" ] availableTiles
        
        let ng1 = 
            ng0
            |> (drawTile Player2 BuildingLvl2)
        match ng1 with
        | Error (NotEnoughTilePoint(cost, points)) -> Assert.AreEqual((2,0), (cost, points))
        | Error e -> Assert.Fail(sprintf "Invalid error, got: %A" e)
        | Success s -> Assert.Fail(sprintf "An error should have occured, got: %A" s)

        let ng2 = 
            ng0
            |> (gain [Gain.Tile] Player2)
            |> (drawTile Player2 BuildingLvl2)
        match ng2 with
        | Error (NotEnoughTilePoint(cost, points)) -> Assert.AreEqual((2,1), (cost, points))
        | Error e -> Assert.Fail(sprintf "Invalid error, got: %A" e)
        | Success s -> Assert.Fail(sprintf "An error should have occured, got: %A" s)
    
    [<Test>]
    member x.``tile should move from world draw pile to player hands when player has enough tile points``() = 
        let availableTiles : Tile list = 
            [ (newBuildingTile Blue 7)
              (newBuildingTile Yellow 5)
              (newBuildingTile Red 9) ]
            |> List.map (fun t -> BuildingTile t)
        
        let ng0 = 
            newGame [ "John"; "Carmen" ] availableTiles
            |> (gain [Gain.Tile] Player2)
            |> (drawTile Player2 BuildingLvl1)
            |> bind (playerStateOf Player2)
        
        match ng0 with
        | Error e -> Assert.Fail(sprintf "No error should have occured, got: %A" e)
        | Success(game, ps) -> 
            Assert.AreEqual([ BuildingTile(newBuildingTile Blue 7) ], ps.tiles)
            Assert.AreEqual(0, ps.nbTilePoint)
            let remainingTilesInGame = 
                [ (newBuildingTile Yellow 5)
                  (newBuildingTile Red 9) ]
                |> List.map (fun t -> BuildingTile t)
            Assert.AreEqual(remainingTilesInGame, game.availableTiles)