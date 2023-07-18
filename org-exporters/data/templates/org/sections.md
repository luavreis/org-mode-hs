::: {.e:bind #heading}
::::{.e:match #sections.level}
:::{.e:case #1}
# []{.e:org.element.headline} {id=!(this.anchor)}
:::
:::{.e:case #2}
## []{.e:org.element.headline} {id=!(this.anchor)}
:::
:::{.e:case #3}
### []{.e:org.element.headline} {id=!(this.anchor)}
:::
:::{.e:case #4}
#### []{.e:org.element.headline} {id=!(this.anchor)}
:::
:::{.e:case #5}
##### []{.e:org.element.headline} {id=!(this.anchor)}
:::
:::{.e:case #6}
###### []{.e:org.element.headline} {id=!(this.anchor)}
:::
::::
:::

:::: {.e:with sections=this}

:::: {.e:match #sections.type}

::: {.e:case #normal}
:::: {.e:sections.list}
# {.e:heading}
# {.e:this.children}
# {.e:this.subsections}
::::
:::

::: {.e:case #over-level}
:::: {.e:pandoc.block.ordered-list style=decimal delim=period}
::::: {.e:sections.list}
:::::: {#list.item}
**[]{.e:org.element.headline}**
# {.e:this.children}
# {.e:this.subsections}
::::::
:::::
::::
:::

::::

::::
