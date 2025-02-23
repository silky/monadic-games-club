module Pages.Home_ exposing (page)

import Html exposing (text, a)
import Html.Attributes as A
import View exposing (View)



page : View msg
page =
    { title = "Homepage"
    , body =
      [ a [ A.href "/skulls" ] [ text "Skulls" ]
      ]
    }
