<table>
  <table:head>
    <thead>
      <head:rows>
        <tr>
          <row:cells>
            <if cell:alignment>
              <th style="text-align: !(cell:alignment);"><cell:content/></th>
              <else/>
              <th><cell:content/></th>
            </if>
          </row:cells>
        </tr>
      </head:rows>
    </thead>
  </table:head>
  <table:bodies>
    <tbody>
      <body:rows>
        <tr>
          <row:cells>
            <if cell:alignment>
              <td style="text-align: !(cell:alignment);"><cell:content/></td>
              <else/>
              <td><cell:content/></td>
            </if>
          </row:cells>
        </tr>
      </body:rows>
    </tbody>
  </table:bodies>
</table>
