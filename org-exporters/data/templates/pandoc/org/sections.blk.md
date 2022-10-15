[ [`!(todo-name)`\ ]{.e:if tag="todo-name"}
  [`[#!(priority)]`\ ]{.e:if tag="priority"}
  []{.e:section:headline}
  [\ [!(tag)]{.smallcaps}]{.e:section:tags}
]{.e:bind tag=headline}

::: e:sections

::: {.e:case tag=normal}
# []{.e:headline} {.e:section:h-n id="!(section:anchor)"}

:::: e:section:children
::::

:::: e:section:subsections
::::
:::

::: {.e:case tag=over-level}
1. **[[]{.e:headline}]{id="!(section:anchor)"}**
 
   :::: e:section:children
   ::::

   :::: e:section:subsections
   ::::
:::

:::
