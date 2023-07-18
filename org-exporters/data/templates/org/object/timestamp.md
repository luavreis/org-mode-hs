[[[, repeating every !(value)!(unit)]{.e:open #this.repeater}]{.e:try}]{.e:bind #repeater}

[[[, warning for !(value)!(unit)]{.e:open #this.warning-period}]{.e:try}]{.e:bind #warning}

[[]{.e:this.date format="%a, %b %e, %Y"}[ at []{.e:this.time format="%R%P"}]{.e:try}]{.e:bind #datetime}

[ [ [ []{.e:datetime}[]{.e:repeater}[]{.e:warning}
    ]{.e:with this=!(caller.attrs.ts)}
  ]{.timestamp}
]{.e:bind #timestamp}

[ [[]{.e:timestamp ts=this}]{.e:case #single}
  [from []{.e:timestamp ts=this.from} to []{.e:timestamp ts=this.to}]{.e:case #range}
]{.e:match #this.span}
