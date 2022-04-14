<bind tag="DefNumberMarker"><LineNumber><span class="linenumber"><Number /></span></LineNumber></bind>

<pre class="code ${Language}"><SrcLines>
    <case tag="plain">
      <code class="line-of-code"><DefNumberMarker /><Contents /></code>
    </case>
    <case tag="ref">
      <span id="${Id}" class="coderef-target">
        <code class="line-of-code"><DefNumberMarker /><Contents /></code><span class="coderef">(<Ref />)</span>
      </span>
    </case>
  </SrcLines>
</pre>
