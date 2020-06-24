{-# LANGUAGE FunctionalDependencies #-}
{-|
Module      : GameSearch.Core
Description : Mostly general-purpose implementation of multiplayer Monte Carlo Tree Search
Copyright   : (c) Avinash Dwarapu, 2017
License     : BSD-3-Clause
Maintainer  : 
Stability   : experimental
Portability : POSIX

Copyright (c) 2017, Avinash Dwarapu

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Avinash Dwarapu nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module GameSearch.Core
    ( Spec(..)
    , Node

    , empty
    , child
    , bestAction
    , monteCarlo
    ) where

import           Data.List                            (find, maximumBy)
import           Data.Map.Strict                      ((!))
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (maybe)
import           Data.Ord                             (comparing)
import           Data.Random                          (sampleState)
import           Data.Random.Distribution.Categorical (weightedCategorical)
import           System.Random                        (StdGen)

-- | A game specfication, containing the state type, action type and player type.
-- Unfortunately, the action and player types must implement Ord, since they
-- are stored in a Map.
class (Ord a, Ord p) => Spec s a p | s -> a, s -> p where
    -- | Returns the legal actions the player can perform from this state,
    -- preceded by their weights, representing how likely the player will play
    -- the state.
    --
    -- A final state has returns [].
    actions :: s -> [(Double, a)]

    -- | The player whose utilities are maximized for this state.
    player :: s -> p

    -- | If the state is a final state, returns a "map" containing pairs of
    -- players and their scores. Each player is assummed to maximize their
    -- payouts.
    payouts :: s -> [(p, Double)]

    -- | Apply an action to a state, returning the resulting state.
    apply :: a -> s -> s

-- | A node in the monte carlo search tree.
data (Ord a, Ord p) => Node a p =
    Node {
      -- | The children of the node. Each edge is an action that results in the
      -- child node.
     children :: Map.Map a (Node a p)
      -- | The mean payouts for each player in the game.
    , meanPayouts :: Map.Map p Double
      -- | The number of times this node was "played".
    , playCount :: Double
    }

--
-- Public Methods
--

-- | Create a blank node.
empty :: (Ord a, Ord p) => Node a p
empty = Node
    { children    = Map.empty
    , meanPayouts = Map.empty
    , playCount   = 0.0
    }

-- | Select a child node from a parent node.
child :: (Ord a, Ord p) => a -> Node a p -> Maybe (Node a p)
child action node = Map.lookup action (children node)

-- | Returns the best action from a given node and state.
bestAction :: Spec s a p => Node a p -> s -> a
bestAction node state =
    fst . maximumBy (comparing snd) . Map.toList $
    Map.map (\child -> meanPayouts child ! player state) (children node)

-- | Exported monte carlo function.
-- Essentially the "select" function, but drops the payout results.
monteCarlo :: Spec s a p => StdGen -> s -> Node a p -> (StdGen, Node a p)
monteCarlo rand state node = fst (select rand state node)

--
-- Monte Carlo Tree Search
--

-- | Run monte carlo simulation, returning the updated node, the updated
-- random generator, and the resulting payout of the simulation.
select :: Spec s a p
             => StdGen -> s -> Node a p
             -> ((StdGen, Node a p), Map.Map p Double)
select rand state node
    | null (actions state) = selectFinal rand state node
    | otherwise =
        maybe
            (selectNext rand state node) -- Perform selection based on UCT
            (expand rand state node)     -- Perform expansion if unexpanded
            (findUnexpanded state node)

-- | Update the node's payouts assuming the state is the final state.
selectFinal :: (Spec s a p)
               => StdGen -> s -> Node a p
               -> ((StdGen, Node a p), Map.Map p Double)
selectFinal rand state node =
    let curPayouts = Map.fromList (payouts state)
    in ((rand, addPayouts curPayouts node), curPayouts)

-- | Recursively call select on a child of this tree based on UCT.
selectNext :: (Spec s a p)
             => StdGen -> s -> Node a p
             -> ((StdGen, Node a p), Map.Map p Double)
selectNext rand state node =
    ((newRand, backprop action newPayouts child node), newPayouts)
    where
        action = uct state node
        ((newRand, child), newPayouts) =
            select rand (apply action state) (children node ! action)

-- | Create a new node and simulate from there.
expand :: (Spec s a p)
             => StdGen -> s -> Node a p -> a
             -> ((StdGen, Node a p), Map.Map p Double)
expand rand state node action = ((newRand, newNode), newPayouts) where
    (newRand, newPayouts) = simulate rand (apply action state)
    newNode = backprop action newPayouts (singleton newPayouts) node

-- | Simulates the game randomly from a starting state.
simulate :: Spec s a p => StdGen -> s -> (StdGen, Map.Map p Double)
simulate rand state
    | null (actions state) = (rand, Map.fromList (payouts state))
    | otherwise            = simulate newRand childState
    where
        childState = apply childAction state
        (childAction, newRand) =
            sampleState (weightedCategorical (actions state)) rand

-- | Update a node with the payout and the updated child node.
backprop :: (Ord a, Ord p) =>
            a -> Map.Map p Double -> Node a p -> Node a p -> Node a p
backprop action payouts child node =
    (addPayouts payouts node)
    { children = Map.insert action child (children node) }

--
-- Utility Functions
--

-- | Create a singleton node.
singleton :: (Ord a, Ord p) => Map.Map p Double -> Node a p
singleton payoutMap =
    Node { children = Map.empty, meanPayouts = payoutMap, playCount = 1.0 }

-- | Looks for an unexpanded action to start simulation from.
-- If it returns Just a, start simulating from (a).
-- If it returns Nothing, continue selecting down using the uct function.
findUnexpanded :: Spec s a p => s -> Node a p -> Maybe a
findUnexpanded state node =
    find (`Map.notMember` children node) (map snd (actions state))

-- | Finds the best action under UCB1 to continue selection.
-- This function assumes that there are no unexpanded nodes or terminal nodes.
uct :: (Spec s a p) => s -> Node a p -> a
uct state node =
    maximumBy (comparing (ucb . (children node !))) (map snd (actions state))
    where
        ucb child = (meanPayouts child ! player state) +
                    sqrt 2 * (sqrt (log (playCount node)) / playCount child)

-- | Update a node with the payout.
addPayouts :: (Ord a, Ord p) => Map.Map p Double -> Node a p -> Node a p
addPayouts payouts node =
    node
    { meanPayouts =
          Map.unionWith (addToMean (playCount node)) payouts (meanPayouts node)
    , playCount = playCount node + 1
    }

-- | Add a number to a mean, given we know how many numbers make up the mean.
addToMean :: Double -> Double -> Double -> Double
addToMean counts number mean = (mean * counts + number) / (counts + 1)
