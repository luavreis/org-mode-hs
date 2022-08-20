::: {.e:bind tag=item}
:::: e:cons

[ [ ☒ ]{.e:case tag=true}
  [ ☐ ]{.e:case tag=false}
  [ ☐ ]{.e:case tag=partial}
]{.e:switch tag=checkbox}

::::: e:list-item-content
:::::

::::
:::

::: e:list-items

:::: {.e:case tag=unordered}
- ::: e:item
  :::
::::

:::: {.e:case tag=ordered-num}
1. ::: e:item
   :::
::::

:::: {.e:case tag=ordered-alpha}
a. ::: e:item
   :::
::::

:::: {.e:case tag=descriptive}
[]{.e:descriptive-tag}
  ~ ::: e:list-item-content
    :::
::::

:::
