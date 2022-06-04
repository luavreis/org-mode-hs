<!-- Checkboxes. All three cases must be defined. -->
<bind tag="Checkbox:true">
  <code>[X]</code>
</bind>
<bind tag="Checkbox:false">
  <code>[ ]</code>
</bind>
<bind tag="Checkbox:partial">
  <code>[-]</code>
</bind>

<!-- Plain list types. All three cases must be defined. -->

<bind tag="PlainList:unordered">
  <bind tag="Bullet:-">disc</bind>
  <bind tag="Bullet:+">square</bind>
  <bind tag="Bullet:*">circle</bind>

  <ul style="list-style-type: ${Bullet};">
    <ListItems>
      <li>
        <Checkbox />
        <Contents />
      </li>
    </ListItems>
  </ul>
</bind>

<bind tag="PlainList:ordered">
  <bind tag="Counter:num">decimal</bind>
  <bind tag="Counter:alpha">lower-alpha</bind>

  <ol style="list-style-type: ${Counter};">
    <ListItems>
      <li value="${CounterSet}">
        <Checkbox />
        <Contents />
      </li>
    </ListItems>
  </ol>
</bind>

<bind tag="PlainList:descriptive">
  <dl>
    <ListItems>
      <dt><Checkbox /><Tag /></dt>
      <dd><Contents /></dd>
    </ListItems>
  </dl>
</bind>

<!-- Here is where the actual rendering takes place. During rendering
     PlainList is replaced by the corresponding type defined above. -->

<WithAffiliated>
  <PlainList />
</WithAffiliated>
