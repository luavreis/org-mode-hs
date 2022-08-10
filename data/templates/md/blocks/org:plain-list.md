::: {#bind tag=item}
:::: {#cons}

[ [ ☒ ]{#case tag=true}
  [ ☐ ]{#case tag=false}
  [ ☐ ]{#case tag=partial}
]{#switch tag=checkbox}

::::: {#list-item-content}
:::::

::::
:::

::: {#list-items}

:::: {#case tag=unordered}
- ::: {#item}
  :::
::::

:::: {#case tag=ordered-num}
1. ::: {#item}
   :::
::::

:::: {#case tag=ordered-alpha}
a. ::: {#item}
   :::
::::

:::: {#case tag=descriptive}
[]{#descriptive-tag}
  ~ ::: {#list-item-content}
    :::
::::

:::
