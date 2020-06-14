module Player exposing (takeTurn)

import Warrior exposing (Warrior)
import Warrior.Coordinate exposing (Coordinate)
import Warrior.Direction as Direction exposing (Direction)
import Warrior.History as History exposing (History)
import Warrior.Map as Map exposing (Map)
import Warrior.Map.Tile as Tile exposing (Tile)


takeTurn : Warrior -> Map -> History -> Warrior.Action
takeTurn warrior map history =
    [ ( "pick up", pickUpItem )
    , ( "investigate", investigate )
    , ( "combat", attackOrRunAway )
    , ( "heal", heal )
    , ( "back to enemy", returnToEnemy )
    , ( "toward exit", moveTowardExit )
    , ( "next unvisited", searchUnvisitedSpace )
    , ( "follow wall", followWall )
    ]
        |> step warrior map history


step : Warrior -> Map -> History -> List ( String, Warrior -> Map -> History -> Warrior.Action ) -> Warrior.Action
step warrior map history steps =
    List.foldl (performActionOr warrior map history) Warrior.Wait steps


{-| You see something shiny on the floor and decide to pick it up.
-}
pickUpItem : Warrior -> Map -> History -> Warrior.Action
pickUpItem warrior map _ =
    if Tile.isItem (Map.lookDown warrior map) then
        Warrior.Pickup

    else
        Warrior.Wait


{-| There's another Warrior in the way, you can't go around them so you'll have to go through them with your sword!

Before your enemy can deal a fatal blow you step away to recover.

-}
attackOrRunAway : Warrior -> Map -> History -> Warrior.Action
attackOrRunAway warrior map history =
    Direction.all
        |> List.concatMap (attackOrRunAwayHelper warrior map history)
        |> performFirstAction


{-| You can see a clear path to something in the distance and decide to investigate, it might be useful.
-}
investigate : Warrior -> Map -> History -> Warrior.Action
investigate warrior map _ =
    if warriorCanTakeAnotherHit warrior then
        Direction.all
            |> List.filterMap (investigateHelper warrior map)
            |> performFirstAction

    else
        Warrior.Wait


{-| Your health is not full and there's no danger in sight, you decide to heal.
-}
heal : Warrior -> Map -> History -> Warrior.Action
heal warrior _ _ =
    if Warrior.health warrior + Warrior.healingPotential warrior <= Warrior.maxHealth warrior then
        let
            _ =
                { health = Warrior.health warrior
                , healing = Warrior.healingPotential warrior
                , max = Warrior.maxHealth warrior
                }
                    |> log "heal"
        in
        Warrior.Heal

    else
        Warrior.Wait


{-| You've finished healing and decide to head back to finish off the warrior that hurt you.
-}
returnToEnemy : Warrior -> Map -> History -> Warrior.Action
returnToEnemy warrior _ history =
    History.previousActions warrior history
        |> returnToEnemyHelp


{-| You can see the exit in the distance, you take a step toward it.
-}
moveTowardExit : Warrior -> Map -> History -> Warrior.Action
moveTowardExit warrior map history =
    nextDirectionsTowardExit warrior map history
        |> List.filterMap (moveInDirection warrior map)
        |> performFirstAction


{-| You see a path you've not tried before and take a step along it.
-}
searchUnvisitedSpace : Warrior -> Map -> History -> Warrior.Action
searchUnvisitedSpace warrior map history =
    let
        visited : List Coordinate
        visited =
            History.previousStates warrior history
                |> List.foldr insertPositionFromPreviousState [ Warrior.position warrior ]
    in
    nextDirectionsToFollowWall warrior history
        |> List.foldl (searchUnvisitedSpaceFolder warrior map visited) []
        |> performFirstAction


{-| Not sure where to go next you follow the wall hoping to find somewhere new to explore.
-}
followWall : Warrior -> Map -> History -> Warrior.Action
followWall warrior map history =
    nextDirectionsToFollowWall warrior history
        |> List.filterMap (moveInDirection warrior map)
        |> performFirstAction



--- HELPERS


attackOrRunAwayHelper : Warrior -> Map -> History -> Direction -> List Warrior.Action
attackOrRunAwayHelper warrior map history direction =
    case Map.look direction warrior map of
        ( _, Tile.Warrior _ ) :: _ ->
            if warriorCanTakeAnotherHit warrior then
                [ Warrior.Attack direction ]

            else
                -- Run Away!
                History.previousActions warrior history
                    |> rewind

        _ ->
            []


investigateHelper : Warrior -> Map -> Direction -> Maybe Warrior.Action
investigateHelper warrior map direction =
    Map.look direction warrior map
        |> List.map Tuple.second
        |> lookToInvestigate direction


filterVisitedLocation : List Coordinate -> Direction -> ( Coordinate, Tile ) -> Maybe (List Warrior.Action)
filterVisitedLocation visited direction ( coordinate, tile ) =
    if Tile.canMoveOnto tile then
        if List.member coordinate visited then
            Nothing

        else
            Just [ Warrior.Move direction ]

    else
        Nothing


insertPositionFromPreviousState : ( Warrior, Map ) -> List Coordinate -> List Coordinate
insertPositionFromPreviousState ( warrior, _ ) list =
    case Warrior.position warrior of
        position ->
            if List.member position list then
                list

            else
                position :: list


returnToEnemyHelp : List Warrior.Action -> Warrior.Action
returnToEnemyHelp actions =
    case actions of
        Warrior.Heal :: (Warrior.Move moveDirection) :: (Warrior.Attack _) :: _ ->
            Warrior.Move (reverseDirection moveDirection)

        Warrior.Heal :: rest ->
            returnToEnemyHelp rest

        _ ->
            Warrior.Wait


nextDirectionsTowardExit : Warrior -> Map -> History -> List Direction
nextDirectionsTowardExit warrior map history =
    ( warrior, map )
        :: History.previousStates warrior history
        |> lookForExit


lookForExit : List ( Warrior, Map ) -> List Direction
lookForExit states =
    lookForExitHelper states ( Direction.all, [] )
        |> Tuple.second


lookForExitHelper : List ( Warrior, Map ) -> ( List Direction, List Direction ) -> ( List Direction, List Direction )
lookForExitHelper states ( search, found ) =
    case states of
        [] ->
            ( search, found )

        ( warrior, map ) :: rest ->
            search
                |> List.foldl (lookForExitHelperFolder warrior map) ( [], found )
                |> lookForExitHelper rest


lookForExitHelperFolder : Warrior -> Map -> Direction -> ( List Direction, List Direction ) -> ( List Direction, List Direction )
lookForExitHelperFolder warrior map direction ( search, found ) =
    if
        Map.look direction warrior map
            |> List.map Tuple.second
            |> List.any Tile.isExit
    then
        ( search, direction :: found )

    else
        ( direction :: search, found )


searchUnvisitedSpaceFolder : Warrior -> Map -> List Coordinate -> Direction -> List Warrior.Action -> List Warrior.Action
searchUnvisitedSpaceFolder warrior map visited direction acc =
    (Map.look direction warrior map
        |> getPathFiltered (filterVisitedLocation visited) direction
    )
        ++ acc


nextDirectionsToFollowWall : Warrior -> History -> List Direction
nextDirectionsToFollowWall warrior history =
    History.previousActions warrior history
        |> nextDirectionsToFollowWallFromPreviousActions


nextDirectionsToFollowWallFromPreviousActions : List Warrior.Action -> List Direction
nextDirectionsToFollowWallFromPreviousActions actions =
    case actions of
        [] ->
            nextDirectionsToFollowWallFromLastDirection Direction.Left

        (Warrior.Move direction) :: _ ->
            nextDirectionsToFollowWallFromLastDirection direction

        _ :: rest ->
            nextDirectionsToFollowWallFromPreviousActions rest


nextDirectionsToFollowWallFromLastDirection : Direction -> List Direction
nextDirectionsToFollowWallFromLastDirection direction =
    case direction of
        Direction.Right ->
            [ Direction.Down, Direction.Right, Direction.Up, Direction.Left ]

        Direction.Down ->
            [ Direction.Left, Direction.Down, Direction.Right, Direction.Up ]

        Direction.Left ->
            [ Direction.Up, Direction.Left, Direction.Down, Direction.Right ]

        Direction.Up ->
            [ Direction.Right, Direction.Up, Direction.Left, Direction.Down ]


moveInDirection : Warrior -> Map -> Direction -> Maybe Warrior.Action
moveInDirection warrior map direction =
    Map.look direction warrior map
        |> List.head
        |> Maybe.map Tuple.second
        |> Maybe.andThen (moveToTile direction)


moveToTile : Direction -> Tile -> Maybe Warrior.Action
moveToTile direction tile =
    if Tile.canMoveOnto tile then
        Just (Warrior.Move direction)

    else
        Nothing


performFirstAction : List Warrior.Action -> Warrior.Action
performFirstAction actions =
    case actions of
        [] ->
            Warrior.Wait

        action :: _ ->
            action


getPathFiltered : (Direction -> ( Coordinate, Tile ) -> Maybe (List return)) -> Direction -> List ( Coordinate, Tile ) -> List return
getPathFiltered filter direction list =
    case list of
        [] ->
            []

        ( coordinate, tile ) :: [] ->
            filter direction ( coordinate, tile )
                |> Maybe.withDefault []

        first :: _ ->
            getPathFiltered filter direction [ first ]


{-| Reverses the last move action.
-}
rewind : List Warrior.Action -> List Warrior.Action
rewind actions =
    case actions of
        [] ->
            []

        (Warrior.Move direction) :: _ ->
            [ Warrior.Move (reverseDirection direction) ]

        _ :: rest ->
            rewind rest


reverseDirection : Direction -> Direction
reverseDirection direction =
    case direction of
        Direction.Left ->
            Direction.Right

        Direction.Right ->
            Direction.Left

        Direction.Up ->
            Direction.Down

        Direction.Down ->
            Direction.Up


lookToInvestigate : Direction -> List Tile -> Maybe Warrior.Action
lookToInvestigate direction list =
    case list of
        [] ->
            Nothing

        -- There's a Warrior in the way, we probably shouldn't investigate
        (Tile.Warrior _) :: _ ->
            Nothing

        -- That Warrior will probably attack before you can grab the item
        (Tile.Item _) :: (Tile.Warrior _) :: _ ->
            Nothing

        -- We don't need to heal if we can get to the exit
        Tile.Exit :: _ ->
            Just (Warrior.Move direction)

        -- This item might be useful
        (Tile.Item _) :: _ ->
            Just (Warrior.Move direction)

        _ :: rest ->
            lookToInvestigate direction rest


warriorCanTakeAnotherHit : Warrior -> Bool
warriorCanTakeAnotherHit warrior =
    Warrior.health warrior > 3



--- REALLY USEFUL


performActionOr : Warrior -> Map -> History -> ( String, Warrior -> Map -> History -> Warrior.Action ) -> Warrior.Action -> Warrior.Action
performActionOr warrior map history ( name, getNextAction ) action =
    case action of
        Warrior.Wait ->
            getNextAction warrior map history
                |> logAction name

        _ ->
            action


logAction : String -> Warrior.Action -> Warrior.Action
logAction message action =
    case action of
        Warrior.Wait ->
            action

        Warrior.Heal ->
            action

        _ ->
            log message action


log : String -> value -> value
log message value =
    value
