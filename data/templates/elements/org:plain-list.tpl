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
  <bind tag="bullet:-">disc</bind>
  <bind tag="bullet:+">square</bind>
  <bind tag="bullet:*">circle</bind>

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
  <bind tag="counter:num">decimal</bind>
  <bind tag="counter:alpha">lower-alpha</bind>

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
