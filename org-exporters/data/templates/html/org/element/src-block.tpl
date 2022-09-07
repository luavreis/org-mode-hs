<bind outer>
  <if starting-number>
    <pre class="code !(language) with-number" affiliated>
      <apply-content />
    </pre>
    <else />
    <pre class="code !(language)" affiliated>
      <apply-content />
    </pre>
  </if>
</bind>

<bind line-number>
  <if number>
    <span class="line-number"><number /></span>
  </if>
</bind>

<bind line-content><if content-pretty><content-pretty /><else /><content /></if></bind>

<bind line-of-code>
  <line-number /><code class="line-of-code"><line-content /><br/></code>
</bind>

<outer>
  <src-lines>
    <case plain>
      <line-of-code />
    </case>
    <case ref>
      <span id="!(id)" class="coderef-target"><line-of-code /><span class="coderef">(<ref />)</span></span>
    </case>
  </src-lines>
</outer>
