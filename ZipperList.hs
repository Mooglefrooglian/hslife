module ZipperList
(  ZipperList,
   goForward,
   goBack,
   getFocus,
   setFocus
) where

type ZipperList a = ([a], [a]) --First is the elements ahead of us, second is the elements behind our focus

goForward :: ZipperList a -> ZipperList a
goForward (x:xs, ys) = (xs, x:ys)

goBack :: ZipperList a -> ZipperList a
goBack (xs, y:ys) = (y:xs, ys)

getFocus :: ZipperList a -> a
getFocus (x:xs, _) = x
getFocus ([], _) = error "Attempted to get head of an empty list"

setFocus :: ZipperList a -> a -> ZipperList a
setFocus (x:xs, ys) a = (a:xs, ys)
