::: {.e:bind #bind-contents}
:::: {.e:if #this.plain}

[[]{.e:this.plain}]{.e:bind #item-contents}

::::: {.e:else}

:::::: {.e:bind #item-contents}
# {.e:this.content}
::::::

:::::
::::
:::

::: {.e:bind #item}
# {.e:bind-contents}
:::: {#list.item}
# {.e:item-contents}
::::
:::

::: {.e:bind #desc-item}
# {.e:bind-contents}
:::: {#list.item}
# []{.e:this.descriptive-tag} {#list.item.term}
::::: {#list.item.definition}
# {.e:item-contents}
:::::
::::
:::

::: {.e:match #this.type}

:::: {.e:case #unordered}
::::: {.e:pandoc.block.bullet-list}
# {.e:this.items.list with=item}
:::::
::::

:::: {.e:case #ordered-num}
::::: {.e:pandoc.block.ordered-list style=decimal}
# {.e:this.items.list with=item}
:::::
::::

:::: {.e:case #ordered-alpha}
::::: {.e:pandoc.block.ordered-list style=lower-alpha}
# {.e:this.items.list with=item}
:::::
::::

:::: {.e:case #descriptive}
::::: {.e:pandoc.block.definition-list}
# {.e:this.items.list with=desc-item}
:::::
::::
:::
