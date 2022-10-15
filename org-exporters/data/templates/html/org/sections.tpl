<bind tag="headline">
  <span class="headline">
    <span class="title">
      <section:headline/>
    </span>
    <if priority>
      <span class="priority !(priority)">Priority: <priority/></span>
    </if>
    <if todo-name>
      <span class="todokw !(todo-state)">
        <todo-name/>
      </span>
    </if>
    <section:tags>
      <span class="tag tag-!(tag)"><tag/></span>
    </section:tags>
  </span>
</bind>

<case normal>
  <sections>
    <section id="!(section:anchor)">
      <section:h-n><headline /></section:h-n>
      <section:children/>
      <section:subsections/>
    </section>
  </sections>
</case>

<case over-level>
  <ol>
    <sections>
      <li>
        <a id="!(section:anchor)"></a><headline /><br />
        <section:children/>
        <section:subsections/>
      </li>
    </sections>
  </ol>
</case>
