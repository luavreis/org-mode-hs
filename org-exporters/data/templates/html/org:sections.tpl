<bind tag="headline">
  <if todo-name>
    <span class="todokw !(todo-state)">
      <todo-name/>
    </span>
  </if>
  <if priority>
    <span class="priority !(priority)">[#<priority/>]</span>
  </if>
  <headline-title/>
  <tags>
    <span class="tag tag-!(tag)"><tag/></span>
  </tags>
</bind>

<case normal>
  <sections>
    <section id="!(anchor)">
      <h-n><section:headline /></h-n>
      <section:children />
      <section:subsections />
    </section>
  </sections>
</case>

<case over-level>
  <ol>
    <sections>
      <li>
        <a id="!(anchor)"></a><section:headline /><br />
        <section:children />
        <section:subsections />
      </li>
    </sections>
  </ol>
</case>
