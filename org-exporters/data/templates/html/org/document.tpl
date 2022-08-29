<!doctype html>
<html>
  <head>
    <meta charset="utf-8" />
    <if kw:title>
      <title><kw:title /></title>
    </if>
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
    <extra:toc>
      <h2>Table of Contents</h2>
    </extra:toc>
    <doc:children />
    <doc:sections />
    <doc:footnotes>
      <br />
      <div id="footnotes">
        <footnote-defs>
          <div class="footdef">
            <a id="fn-!(footnote-def:number)" href="#fnr-!(footnote-def:number)">[<footnote-def:number />]</a>
            <footnote-def:content />
          </div>
        </footnote-defs>
      </div>
    </doc:footnotes>
  </body>
</html>
