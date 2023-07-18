::: {.e:bind #content}
:::: {.e:if #this.plain}
# {.e:this.plain}
::::: {.e:else}
# {.e:this.content}
:::::
::::
:::

::: {.e:bind #item}
:::: {#list.item}
# {.e:content}
::::
:::

::: {.e:bind #desc-item}
:::: {#list.item}
# []{.e:this.descriptive-tag} {#list.item.term}
::::: {#list.item.definition}
# {.e:content}
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
