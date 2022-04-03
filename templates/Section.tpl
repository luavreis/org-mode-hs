<ignore>
You should bind the following splices:
 - TodoKw:todo :: Will be rendered by <TodoKw /> splice if keyword state is "TODO"
 - TodoKw:done :: Will be rendered by <TodoKw /> splice if keyword state is "DONE"
Available splices:
 - Headline    :: Will render its content either inside <hN></hN> tags or with a <br/> after, according to orgExportHeadlineLevels.
  + TodoKw     :: Renders either nothing, TodoKw:todo or TodoKw:done (see above).
   * TodoName  :: The name of TODO keyword.
  + Priority   :: Content is rendered only if the headline has a priority cookie.
   * Value     :: Priority cookie value (number or A, B, C,...)
  + Title      :: Headline title.
  + Tags       :: Content inside will be rendered for each tag.
   * Tag       :: Tag name.
 - Anchor      :: Section anchor.
 - Contents    :: Section contents (before child headlines).
 - Subsections :: Section subsections.
</ignore>

<bind tag="TodoKw:todo">
  <span class="todokw todo">
    <TodoName />
  </span>
</bind>

<bind tag="TodoKw:done">
  <span class="todokw done">
    <TodoName />
  </span>
</bind>

<section id="${Anchor}">
  <Headline>
    <TodoKw />
    <Priority>
      <span class="priority ${Value}">[#<Value />]</span>
    </Priority>
    <Title />
    <Tags>
      <span class="tag ${Tag}"><Tag /></span>
    </Tags>
  </Headline>

  <Contents />
  <Subsections />
</section>
