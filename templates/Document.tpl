<ignore>
Available splices:
 - Language  :: Language, from the Org file.
 - Title     :: Title, from the Org file.
 - Date      :: Date, from the Org file.
 - Author    :: Author, from the Org file.
 - Contents  :: Top-level contents for the file.
 - Sections  :: Sections that are children of the top-level section, i.e. contents after the first headline.
 - Footnotes :: Content inside this splice will be rendered only if there are one or more referenced footnote definitions.
  + FootnoteDefs :: Content inside this splice will be rendered for each footnote definition.
   * Number   :: The number assigned to the footnote definition.
   * Contents :: The footnote definition contents.
</ignore>

<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="${Language}" lang="${Language}">
  <head>
    <meta charset="utf-8" />
    <meta name="generator" content="hs-org-parser" />
    <title><Title /></title>
    <style type="text/css" media="screen">
     .todokw {
         font-family: monospace;
     }
     .todokw.todo {
         color: red
     }
     .todokw.done {
         color: green
     }
     .linenumber {
         display: inline-block;
         float: left;
         padding-left: auto;
         margin-left: auto;
         text-align: right;
     }
     .line-of-code {
         float: left;
         clear: right;
     }
     .coderef {
         float: right;
     }
    </style>
  </head>
  <body>
    <Contents />
    <Footnotes />
  </body>
</html>
