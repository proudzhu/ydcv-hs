module Formatter (renderDict, renderDictLn)  where

import Data.Maybe (fromJust, fromMaybe)
import Text.PrettyPrint.ANSI.Leijen
import Data.List (intercalate)
import YdResponse

{-
dict :: Dict
dict = Dict {translation = ["\22909"], basic = BasicDict {ukSpeech = "http://fanyi.youdao.com/openapi.do?type=data&voice=true&version=1.2&key=659600698&keyfrom=YouDaoCV&q=good&vt=1", usPhonetic = "g\650d", speech = "http://fanyi.youdao.com/openapi.do?type=data&voice=true&version=1.2&key=659600698&keyfrom=YouDaoCV&q=good&vt=1", phonetic = "g\650d", ukPhonetic = "g\650d", usSpeech = "http://fanyi.youdao.com/openapi.do?type=data&voice=true&version=1.2&key=659600698&keyfrom=YouDaoCV&q=good&vt=2", explains = ["n. \22909\22788\65307\21892\34892\65307\24951\24936\30340\34892\20026","adj. \22909\30340\65307\20248\33391\30340\65307\24841\24555\30340\65307\34388\35802\30340","adv. \22909","n. (Good)\20154\21517\65307(\33521)\21476\24503\65307(\29790\20856)\25096\24503"]}, query = "good", errorCode = 0, web = [WebDict {value = ["\22909","\21892","\21830\21697"], key = "GOOD"},WebDict {value = ["\32822\31267\21463\38590\33410","\32822\31267\21463\38590\26085","\21463\38590\33410"], key = "Good Friday"},WebDict {value = ["\50948\53412\48177\44284 \46041\51020\51060\51032\50612 \47928\49436","Good Time","Good Time"], key = "Good Time"}]}
-}

renderDictLn :: Dict -> IO ()
renderDictLn dict = putDoc (renderDict dict)

renderDict :: Dict -> Doc
renderDict dict =
           case errorCode dict of
              0 -> renderDictHelper dict
              _ -> text " -- No result for this query.\n"

renderDictHelper :: Dict -> Doc
renderDictHelper dict = renderNormalDict dict <+>
                        renderBasicDict (basic dict) <+>
                        renderWebDict (web dict)

renderNormalDict :: Dict -> Doc
renderNormalDict dict = underline (text (query dict))

renderBasicDict :: Maybe BasicDict -> Doc
renderBasicDict maybeBasic = case maybeBasic of
                               Just basic -> renderBasicDictJust basic
                               Nothing -> text ""

renderBasicDictJust :: BasicDict -> Doc
renderBasicDictJust basic = text " UK: [" <+>
                        yellow (text (fromMaybe "" (ukPhonetic basic))) <+>
                        text "], US: [" <+>
                        yellow (text (fromMaybe "" (usPhonetic basic))) <+>
                        text "]" <+> hardline <+>
                        cyan (text "  Text to Speech:\n") <+>
                        text "     * UK:" <+> text (fromMaybe "" (ukSpeech basic)) <+> hardline <+>
                        text "     * US:" <+> text (fromMaybe "" (usSpeech basic)) <+> hardline <+>
                        hardline <+>
                        cyan (text "  Word Explanation:") <+> hardline <+>
                        text "     *" <+>
                        text (intercalate "\n      * " (explains basic)) <+> hardline <+>
                        hardline

renderWebDict :: Maybe [WebDict] -> Doc
renderWebDict maybeWebs = case maybeWebs of
                            Just webs -> renderWebDictJust webs
                            Nothing   -> text ""

renderWebDictJust :: [WebDict] -> Doc
renderWebDictJust webs = cyan (text "  Web Reference:") <+> hardline <+>
                     concatDoc (map renderWebDictHelper webs) <+> hardline


concatDoc :: [Doc] -> Doc
concatDoc (x:xs) = x <+> concatDoc xs
concatDoc _      = text ""

renderWebDictHelper :: WebDict -> Doc
renderWebDictHelper web = text "     *" <+> yellow (text (key web)) <+> hardline <+>
                          text "        " <+>
                          text (intercalate "; " (value web)) <+> hardline
