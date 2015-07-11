module core.test

open System
open NUnit.Framework
open ginkgopolis.core

[<TestFixture>]
type ``mapPlayers function``() = 
    
    [<Test>]
    member x.``should fail when trying to map more than 5 players ``() = 
        match mapPlayers [ "John"; "Carmen"; "Pacman"; "Flibuste"; "Colin"; "Martin" ] AllPlayerIds Map.empty with
        | Error(TooMuchPlayer ps) -> Assert.AreEqual([ "Martin" ], ps)
        | Error x -> Assert.Fail(sprintf "Wrong error: %A" x)
        | Success _ -> Assert.Fail "More than 5 players should bot be allowed"
    
    [<Test>]
    member x.``should associate player with playerId``() = 
        match mapPlayers [ "Carmen"; "Pacman" ] AllPlayerIds Map.empty with
        | Error x -> Assert.Fail(sprintf "No error expected: %A" x)
        | Success mapped -> 
            let expected = 
                [ ("Carmen", Player1)
                  ("Pacman", Player2) ]
                |> Map.ofList
            Assert.AreEqual(expected, mapped)

[<TestFixture>]
type ``triggered action``() = 
    
    [<Test>]
    member x.``world should not be updated when action triggered and callback types do not match``() = 
        let character1OnAction = whenAction ActionKind.Urbanization (increase IncreasableItem.Resource)
        let game = newGame [ "John"; "Carmen" ] []
        Assert.AreEqual(game, ((character1OnAction ActionKind.EndGame) Player1 game))
        Assert.AreEqual(game, ((character1OnAction ActionKind.FloorConstruction) Player1 game))
    
    [<Test>]
    member x.``increase number of resources should fail when none are available``() = 
        let character1OnAction = whenAction ActionKind.Urbanization (increase IncreasableItem.Resource)
        let game = newGame [ "John"; "Carmen" ] []
        let ng = (character1OnAction ActionKind.Urbanization) Player1 game
        match (withinPlayerStateOf Player1 ng (fun p -> p.nbResource)) with
        | Error e -> Assert.AreEqual(NoResourceAvailable, e)
        | Success v -> Assert.Fail(sprintf "And error should have occured, got: %A" v)
    
    [<Test>]
    member x.``increase number of availables resource should increment its count``() = 
        let ng0 = newGame [ "John"; "Carmen" ] []
        let ng1 = increase IncreasableItem.AvailableResource Player2 ng0
        let ng2 = increase IncreasableItem.AvailableResource Player2 ng1
        match (withinPlayerStateOf Player2 ng2 (fun p -> p.nbResourceAvailable)) with
        | Error e -> Assert.Fail(sprintf "No error should have occured, got: %A" e)
        | Success v -> Assert.AreEqual(2, v)
    
    [<Test>]
    member x.``increase number of resources should consume available resource``() = 
        let character1OnAction = whenAction ActionKind.Urbanization (increase IncreasableItem.Resource)
        let ng0 = newGame [ "John"; "Carmen" ] []
        let ng1 = increase IncreasableItem.AvailableResource Player2 ng0
        let ng2 = (character1OnAction ActionKind.Urbanization) Player2 ng1
        match (withinPlayerStateOf Player2 ng2 (fun p -> p.nbResource)) with
        | Error e -> Assert.Fail(sprintf "No error should have occured, got: %A" e)
        | Success v -> Assert.AreEqual(1, v)
    
    [<Test>]
    member x.``increase tile should move the first tile available from game to player's hand``() = 
        let character2OnAction = whenAction ActionKind.Urbanization (increase IncreasableItem.BuildingTile)
        
        let availableTiles : Tile list = 
            [ (newBuildingTile Blue 7)
              (newBuildingTile Yellow 5)
              (newBuildingTile Red 9) ]
            |> List.map (fun t -> BuildingTile t)
        
        let ng0 = newGame [ "John"; "Carmen" ] availableTiles
        let ng2 = (character2OnAction ActionKind.Urbanization) Player2 ng0
        // Check player's hand
        let player2Tiles = withinPlayerStateOf Player2 ng2 (fun p -> p.tiles)
        match player2Tiles with
        | Error e -> Assert.Fail(sprintf "No error should have occured, got: %A" e)
        | Success v -> 
            match (List.head v) with
            | BuildingTile t -> Assert.AreEqual((newBuildingTile Blue 7), t)
            | x -> Assert.Fail(sprintf "Invalid tile type, got: %A" x)
        // Check game's available tiles
        match ng2 with
        | Error e -> Assert.Fail(sprintf "No error should have occured, got: %A" e)
        | Success g -> 
            let expectedTiles = 
                [ (newBuildingTile Yellow 5)
                  (newBuildingTile Red 9) ]
                |> List.map (fun t -> BuildingTile t)
            Assert.AreEqual(expectedTiles, g.availableTiles)
