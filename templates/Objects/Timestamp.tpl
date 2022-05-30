<bind tag="Unit:h">hour</bind>
<bind tag="Unit:d">day</bind>
<bind tag="Unit:w">week</bind>
<bind tag="Unit:m">month</bind>
<bind tag="Unit:y">year</bind>

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
<bind tag="DefaultTimestamp"><time><TSDate>%a, %B %e, %Y</TSDate><TSTime> at %R %P</TSTime></time><Repeater>, repeating every <Value /> <Unit /></Repeater><WarningPeriod>, warning for <Value /> <Unit /></WarningPeriod></bind>

<bind tag="Timestamp:active:single">(<DefaultTimestamp/>)</bind>

<bind tag="Timestamp:inactive:single"><span class="inactive"><Timestamp:active:single/></span></bind>

<bind tag="Timestamp:active:range">(<From>from <DefaultTimestamp/></From><To>to <DefaultTimestamp /></To>)</bind>

<bind tag="Timestamp:inactive:range"><span class="inactive"><Timestamp:active:range/></span></bind>

<span class="timestamp"><Timestamp /></span>
