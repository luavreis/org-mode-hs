<bind table-row>
  <tr>
    <row:cells>
      <if cell:alignment>
        <th style="text-align: !(cell:alignment);"><cell:content/></th>
        <else/>
        <th><cell:content/></th>
      </if>
    </row:cells>
  </tr>
</bind>

<table>
  <table:head>
    <thead>
      <head:rows>
        <table-row/>
      </head:rows>
    </thead>
  </table:head>
  <table:bodies>
    <tbody>
      <body:rows>
        <table-row/>
      </body:rows>
    </tbody>
  </table:bodies>
</table>
