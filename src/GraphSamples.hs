module GraphSamples where

import Data.Graph

eilerGraph1 :: Graph
eilerGraph1 = buildG (0,3) [(0,1), (1,0), (1,2), (1,3), (2,1), (2,3), (3,1), (3,2)]

eilerGraph2 :: Graph
eilerGraph2 = buildG (0,6) [(0,1), (1,0),(1,5), (2,5),(2,3), (3,2),(3,5), (4,5),(4,6), (5,1),(5,2),(5,3),(5,4), (6,4)]

eilerGraph3 :: Graph
eilerGraph3 = buildG (0,4) [(0,1),(0,2),(0,3), (1,0),(1,2),(1,4), (2,0),(2,1), (3,0),(3,4), (4,1), (4,3)]

hamiltonGraph1 :: Graph
hamiltonGraph1 = buildG (0,3) [(0,1), (0,2), (0,3), (1,0), (1,2), (1,3), (2,0), (2,1), (2,3), (3,0), (3,1), (3,2)]

hamiltonGraph2 :: Graph
hamiltonGraph2 = buildG (0,5) [(0,1), (0,3), (0,4), (1,0), (2,3), (2,5), (3,0), (3,2), (3,4), (3,5), (4,0), (4,3), (5,2),(5,3)]

hamiltonGraph3 :: Graph
hamiltonGraph3 = buildG (0,5) [(0,1),(0,2), (1,0),(1,2),(1,3),(1,5), (2,0),(2,1), (3,1),(3,4), (4,3),(4,5), (5,1),(5,4)]
