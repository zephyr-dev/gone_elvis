module Client.UI.Env (setDeploy) where

import           Control.Monad   (forM)
import           Data.Foldable   (foldMap)
import           Haste.App       (Client, MonadIO, liftIO, Event(..), onServer, (<.>))
import           Haste.DOM       (Elem)
import           Haste.Perch.Client     (Perch, a, atr, build, div, h4, li, p, span,
                                  table, tbody, td, tr, ul, (!), this, addEvent)
import           Haste.Prim
import           Haste.Serialize
import           Prelude         hiding (div, span, (!))

import           Types.DeployEvent
import           Types.API (API(..))


setDeploy :: API -> Elem -> Int -> DeployEvent -> Client ()
setDeploy api envContainer n deploy = build (setDeployPerch api deploy n) envContainer >> return ()

setDeployPerch :: API -> DeployEvent -> Int -> Perch
setDeployPerch api deploy n = do
  div ! atr "class" "panel panel-default" $ p "HELLO"

  {- div ! atr "class" "panel panel-default" $ do -}
    {- div ! atr "class" "panel-heading" $ do -}
      {- h4 ! atr "class" "panel-title clearfix" $ do -}
        {- div ! atr "data-toggle" "collapse" ! atr "href" (genHref "collapse" n) $ do -}
          {- div ! atr "class" "request" $ do -}
            {- span ! atr "class" (addVerbClass req "label") $ verb req -}
            {- span ! atr "class" "path text-muted small" $ path req -}
            {- span ! atr "class" "timestamp pull-right text-muted small" $ timestamp req -}
    {- div ! atr "id" (genId "collapse" n) ! atr "class" "panel-collapse collapse" $ do -}
      {- div ! atr "class" "panel-body" $ do -}
        {- ul ! atr "id" "tab-test" ! atr "class" "nav nav-pills" $ do -}
          {- li ! atr "class" "active" ! atr "data-toggle" "pill" $ do -}
            {- a ! atr "href" (genHref "general_info" n)       $ "General Info" -}
          {- li ! atr "data-toggle" "pill" $ do -}
            {- a ! atr "href" (genHref "rendered_templates" n) $ "Rendered Templates" -}
          {- li ! atr "data-toggle" "pill" $ do -}
            {- a ! atr "href" (genHref "sql_queries" n)                $ "SQL" -}


        {- div ! atr "class" "tab-content" $ do -}
          {- div ! atr "class" "tab-pane active" ! atr "id" (genId "general_info" n)       $ generalInfoTable req -}
          {- div ! atr "class" "tab-pane"        ! atr "id" (genId "rendered_templates" n) $ renderedTemplatesTable api req -}
          {- div ! atr "class" "tab-pane"        ! atr "id" (genId "sql_queries" n)        $ sqlQueriesTable req -}

  {- where -}
    {- genHref s n = "#" ++ genId s n -}
    {- genId s n = s ++ "_" ++ show n -}

    {- generalInfoTable req = do -}
      {- table ! atr "class" "table general" $ do -}
        {- tbody $ do -}
          {- tr $ do -}
            {- td "Method" -}
            {- td $ verb req -}
          {- tr $ do -}
            {- td "Controller" -}
            {- td $ controller req -}
          {- tr $ do -}
            {- td "Action" -}
            {- td $ action req -}
          {- tr $ do -}
            {- td "Path" -}
            {- td $ path req -}
          {- tr $ do -}
            {- td "Status" -}
            {- td $ statusCode req -}

    {- renderedTemplatesTable :: API -> Request -> Perch -}
    {- renderedTemplatesTable api req = do -}
      {- table ! atr "class" "table templates" $ do -}
        {- tbody $ do -}
          {- foldMap (renderPartial api) $ renderedPartials req -}
      {- where -}
        {- renderPartial api partial = do -}
          {- tr $ do -}
            {- td $ do -}
              {- this `addEvent` OnClick $ \_ _ -> onServer $ (performActionInVim api) <.> (prPath partial) -}
              {- span $ prPath partial -}
            {- td $ prTimestamp partial -}

    {- sqlQueriesTable req = do -}
      {- table ! atr "class" "table sql" $ do -}
        {- tbody $ do -}
          {- foldMap renderSqlQuery $ sqlQueries req -}
      {- where -}
        {- renderSqlQuery query = do -}
          {- tr $ do -}
            {- td ! atr "class" "query" $ sqSql query -}
            {- td $ sqTimestamp query -}


    {- addVerbClass req classes =  classes ++ " " ++ (verbCssClass $ verb req) -}

    {- verbCssClass request = case verb req of -}
      {- "GET"     -> "get" -}
      {- "POST"    -> "post" -}
      {- "PUT"     -> "put" -}
      {- "DELETE"  -> "delete" -}
      {- _         -> "unexpected_verb" -}




