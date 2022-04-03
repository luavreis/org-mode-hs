<ignore>
Available splices:
 - Language      :: Language, from the Org file.
 - Title         :: Title, from the Org file.
 - Date          :: Date, from the Org file.
 - Author        :: Author, from the Org file.
 - Contents      :: Top-level contents for the file.
 - Sections      :: Sections that are children of the top-level section, i.e. contents after the first headline.
 - Footnotes     :: Content inside this splice will be rendered only if there are one or more referenced footnote definitions.
  + FootnoteDefs :: Content inside this splice will be rendered for each footnote definition.
   * Number      :: The number assigned to the footnote definition.
   * Contents    :: The footnote definition contents.
</ignore>

<Contents />
<Sections />
<Footnotes>
  <div id="footnotes">
    <h2>Footnotes: </h2>
    <FootnoteDefs>
      <div class="footdef">
        <sup>
          <a id="fn.${Number}" href="#fnr.${Number}">[<Number />]</a>
        </sup>
        <Contents />
      </div>
    </FootnoteDefs>
  </div>
</Footnotes>
