module App.Form.Field where

import Prelude

import App.Form.Validation (errorToString)
import App.Form.Validation as V
import App.Utils (maybeElement)
import DOM.HTML.Indexed (HTMLinput)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant)
import Formless as F
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Row as Row

submit :: ∀ form act slots m. String -> F.ComponentHTML form act slots m
submit buttonText =
    HH.button
        [ HE.onClick \_ -> Just F.submit
        ]
        [ HH.text buttonText ]

input
    :: ∀ form act slots m sym fields inputs out t0 t1
    . IsSymbol sym
    => Newtype (form Record F.FormField) { | fields }
    => Newtype (form Variant F.InputFunction) (Variant inputs)
    => Row.Cons sym (F.FormField V.FormError String out) t0 fields
    => Row.Cons sym (F.InputFunction V.FormError String out) t1 inputs
    => SProxy sym
    -> form Record F.FormField
    -> Array (HH.IProp HTMLinput (F.Action form act))
    -> F.ComponentHTML form act slots m
input sym form props =
    HH.fieldset_
        [ HH.input
            ( append
                [ HP.value $ F.getInput sym form
                , HE.onValueInput $ Just <<< F.setValidate sym
                ]
                props
            )
        , maybeElement (F.getError sym form) \error ->
            HH.div_
              [ HH.text $ errorToString error ]
        ]
