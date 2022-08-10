<bind tag="headline">
  <if-bound todo-name>
    <span class="todokw !(todo-state)">
      <todo-name/>
    </span>
  </if-bound>
  <if-bound priority>
    <span class="priority !(priority)">[#<priority/>]</span>
  </if-bound>
  <headline-title/>
  <tags>
    <span class="tag tag-!(tag)"><tag/></span>
  </tags>
</bind>

<case normal>
  <sections>
    <section id="!(anchor)">
      <h-n><headline /></h-n>
      <children/>
      <subsections/>
    </section>
  </sections>
</case>

<case over-level>
  <ol>
    <sections>
      <li>
        <a id="!(anchor)"></a><headline /><br />
        <children/>
        <subsections/>
      </li>
    </sections>
  </ol>
</case>
