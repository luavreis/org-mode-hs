<bind tag="checkbox:true">
  <code>[X]</code>
</bind>
<bind tag="checkbox:false">
  <code>[ ]</code>
</bind>
<bind tag="checkbox:partial">
  <code>[-]</code>
</bind>

<case unordered>
  <bind-text tag="bullet:-">disc</bind-text>
  <bind-text tag="bullet:+">square</bind-text>
  <bind-text tag="bullet:*">circle</bind-text>

  <ul style="list-style-type: ${bullet};" affiliated>
    <list-items>
      <li>
        <checkbox />
        <contents />
      </li>
    </list-items>
  </ul>
</case>

<case ordered>
  <bind-text tag="counter:num">decimal</bind-text>
  <bind-text tag="counter:alpha">lower-alpha</bind-text>

  <ol style="list-style-type: ${counter};" affiliated>
    <list-items>
      <li value="${counter-set}">
        <checkbox />
        <contents />
      </li>
    </list-items>
  </ol>
</case>

<case descriptive>
  <dl affiliated>
    <list-items>
      <dt><checkbox /><tag /></dt>
      <dd><contents /></dd>
    </list-items>
  </dl>
</case>
