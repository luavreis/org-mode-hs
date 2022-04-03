<bind tag="DefaultNumberMarker">
  <LineNumberMarker>
    <span class="linenumber"><Number /></span>
  </LineNumberMarker>
</bind>

<bind tag="SrcLine:plain">
  <DefaultNumberMarker /><code class="line-of-code"><Contents/></code>
</bind>

<bind tag="SrcLine:ref">
  <span id="${Id}">
    <DefaultNumberMarker /><code class="line-of-code"><Contents /></code><span class="coderef">(<Ref />)</span>
  </span>
</bind>

<pre class="code ${Language}">
  <SrcLines />
</pre>
