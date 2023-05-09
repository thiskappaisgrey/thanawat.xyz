-- |

module Page.Post where
import Lucid
import Types (Post(..))
import Development.Shake.FilePath (hasTrailingPathSeparator)
  
postList :: [Post] -> Html ()
postList postList = let
  postItem :: Post -> Html ()
  postItem post = li_ $ a_ [href_ $ "/" <> url post, class_ "w-full p-4 block text-amber-400 hover:bg-slate-700"] $ do
    toHtml $ title post
    span_ [class_ "float-right"] $ toHtml $ date post
  in
  div_  [class_ "flex flex-col"] $ do
    h1_ [class_ "text-center text-4xl m-4"] "Blog"
    ul_ [class_ "w-1/3 place-self-center"] $ mapM_ postItem postList
    

