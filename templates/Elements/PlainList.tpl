<!-- Helper for the checkboxes -->
<bind tag="cbwrapper">
  <pre><apply-contents /></pre>&ensp;
</bind>

<!-- Checkboxes. All three cases must be defined. -->
<bind tag="Checkbox:true">
  <cbwrapper>[X]</cbwrapper>
</bind>
<bind tag="Checkbox:false">
  <cbwrapper>[ ]</cbwrapper>
</bind>
<bind tag="Checkbox:partial">
  <cbwrapper>[-]</cbwrapper>
</bind>

<!-- Plain list types. All three cases must be defined. -->

<bind tag="PlainList:unordered">
  <bind tag="Bullet:-">disk</bind>
  <bind tag="Bullet:+">square</bind>
  <bind tag="Bullet:*">circle</bind>

  <ul type="${Bullet}">
    <ListItems>
      <li>
        <Checkbox />
        <Contents />
      </li>
    </ListItems>
  </ul>
</bind>

<bind tag="PlainList:ordered">
  <bind tag="Counter:num">1</bind>
  <bind tag="Counter:alpha">a</bind>

  <ol type="${Counter}">
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
      <dt><Checkbox /><Tag /><dt>
      <dd><Contents /></dd>
    </ListItems>
  </dl>
</bind>

<!-- Here is where the actual rendering takes place. During rendering
     PlainList is replaced by the corresponding type defined above. -->

<WithAffiliated>
  <PlainList />
</WithAffiliated>
