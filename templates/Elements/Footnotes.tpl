<ignore>
- Footnotes     :: Content inside this splice will be rendered only if there are one or more referenced footnote definitions.
 + FootnoteDefs :: Content inside this splice will be rendered for each footnote definition.
  * Number      :: The number assigned to the footnote definition.
  * Contents    :: The footnote definition contents.
</ignore>

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
