module HOC.Utilities where

import HOC.ExportClass ( getIVar )

x # f = f x

x #. v = x # getIVar v
