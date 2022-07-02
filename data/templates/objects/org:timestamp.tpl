<bind tag="unit:h">hour</bind>
<bind tag="unit:d">day</bind>
<bind tag="unit:w">week</bind>
<bind tag="unit:m">month</bind>
<bind tag="unit:y">year</bind>

<!-- Since we will be reusing the same formatting for most cases,
     we define a "default" datetime format for convenience. -->

<!-- TSDate and TSTime tags accepts any Unix time format string,
     and formats the datetime in context (see below) into plain text.

     The reason they are separate is because you can have timestamps
     that do not set the time in Org, so TSTime may not be rendered.

     There are three contexts: for normal timestamps, one single
     global context, and for range timestamps, two contexts that
     are accessed inside the <From></From> and <To></To> tags.

     You can configure the locale via attributes as below.
     Attribute values are separated by commas,
     whitespace between commas and words is removed. -->
<bind tag="default-timestamp"><time><ts-date>%a, %B %e, %Y</ts-date><ts-time> at %R %P</ts-time></time><repeater>, repeating every <value/> <unit/></repeater><warning-period>, warning for <value/> <unit/></warning-period></bind>

<case active-single>(<default-timestamp/>)</case>

<case inactive-single><span class="inactive">(<default-timestamp/>)</span></case>

<case active-range>(<from>from <default-timestamp/></from><to>to <default-timestamp /></to>)</case>

<case inactive-range><span class="inactive">(<from>from <default-timestamp/></from><to>to <default-timestamp /></to>)</span></case>
