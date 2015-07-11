﻿module core.test

open System
open NUnit.Framework
open ginkgopolis.core

[<TestFixture>]
type ``urbanization tokens``() =

    [<Test>]
    member x.``there should be 12 differents urbanization tokens``() =
        let tokens = AllUrbanizationTokens
        Assert.AreEqual (12, List.length tokens)
        Assert.AreEqual (12, Set.count (Set.ofList tokens))

[<TestFixture>]
type ``building tiles``() =

    [<Test>]
    member x.``there should be 1 to 3 for blue, yellow, red building tiles in initial layout``() =
        let eq (bt:BuildingTile) = (fun other -> other = bt)
        let tiles = initialBuildingTiles
        Assert.AreEqual(9, List.length tiles)
        Assert.IsTrue (List.exists (eq (newBuildingTile Blue 1)) tiles)
        Assert.IsTrue (List.exists (eq (newBuildingTile Blue 2)) tiles)
        Assert.IsTrue (List.exists (eq (newBuildingTile Blue 3)) tiles)
        Assert.IsTrue (List.exists (eq (newBuildingTile Red 1)) tiles)
        Assert.IsTrue (List.exists (eq (newBuildingTile Red 2)) tiles)
        Assert.IsTrue (List.exists (eq (newBuildingTile Red 3)) tiles)
        Assert.IsTrue (List.exists (eq (newBuildingTile Yellow 1)) tiles)
        Assert.IsTrue (List.exists (eq (newBuildingTile Yellow 2)) tiles)
        Assert.IsTrue (List.exists (eq (newBuildingTile Yellow 3)) tiles)


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
        let character1OnAction = whenAction ActionKind.Urbanization (gain Gain.Resource)
        let game = newGame [ "John"; "Carmen" ] []
        Assert.AreEqual(game, ((character1OnAction ActionKind.EndGame) Player1 game))
        Assert.AreEqual(game, ((character1OnAction ActionKind.FloorConstruction) Player1 game))
    
    [<Test>]
    member x.``increase number of resources should fail when none are available``() = 
        let character1OnAction = whenAction ActionKind.Urbanization (gain Gain.Resource)
        let game = newGame [ "John"; "Carmen" ] []
        let ng = (character1OnAction ActionKind.Urbanization) Player1 game
        match (withinPlayerStateOf Player1 ng (fun p -> p.nbResource)) with
        | Error e -> Assert.AreEqual(NoResourceAvailable, e)
        | Success v -> Assert.Fail(sprintf "An error should have occured, got: %A" v)
    
    [<Test>]
    member x.``increase number of availables resource should increment its count``() = 
        let ng0 = newGame [ "John"; "Carmen" ] []
        let ng1 = gain Gain.AvailableResource Player2 ng0
        let ng2 = gain Gain.AvailableResource Player2 ng1
        match (withinPlayerStateOf Player2 ng2 (fun p -> p.nbResourceAvailable)) with
        | Error e -> Assert.Fail(sprintf "No error should have occured, got: %A" e)
        | Success v -> Assert.AreEqual(2, v)
    
    [<Test>]
    member x.``increase number of resources should consume player's available resource``() = 
        let character1OnAction = whenAction ActionKind.Urbanization (gain Gain.Resource)
        let ng0 = newGame [ "John"; "Carmen" ] []
        let ng1 = gain Gain.AvailableResource Player2 ng0
        let ng2 = (character1OnAction ActionKind.Urbanization) Player2 ng1
        match (withinPlayerStateOf Player2 ng2 (fun p -> p.nbResource)) with
        | Error e -> Assert.Fail(sprintf "No error should have occured, got: %A" e)
        | Success v -> Assert.AreEqual(1, v)
    
    [<Test>]
    member x.``increase number of success point should work!``() = 
        let character1OnAction = whenAction ActionKind.Urbanization (gain Gain.SuccessPoint)
        let ng0 = newGame [ "John"; "Carmen" ] []
        let ng2 = (character1OnAction ActionKind.Urbanization) Player2 ng0
        match (withinPlayerStateOf Player2 ng2 (fun p -> p.nbSuccessPoint)) with
        | Error e -> Assert.Fail(sprintf "No error should have occured, got: %A" e)
        | Success v -> Assert.AreEqual(1, v)
    
    [<Test>]
    member x.``increase tile should move the first tile available from game to player's hand``() = 
        let character2OnAction = whenAction ActionKind.Urbanization (gain Gain.Tile)
        
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
        let player2Tiles = withinPlayerStateOf Player2 ng2 (fun p -> p.tiles)
        match player2Tiles with
        | Error e -> Assert.Fail(sprintf "No error should have occured, got: %A" e)
        | Success v -> 
            match (List.head v) with
            | BuildingTile t -> Assert.AreEqual((newBuildingTile Blue 7), t)
            | x -> Assert.Fail(sprintf "Invalid tile type, got: %A" x)
        //
        // Check game's available tiles
        //
        match ng2 with
        | Error e -> Assert.Fail(sprintf "No error should have occured, got: %A" e)
        | Success g -> 
            let expectedTiles = 
                [ (newBuildingTile Yellow 5)
                  (newBuildingTile Red 9) ]
                |> List.map (fun t -> BuildingTile t)
            Assert.AreEqual(expectedTiles, g.availableTiles)
