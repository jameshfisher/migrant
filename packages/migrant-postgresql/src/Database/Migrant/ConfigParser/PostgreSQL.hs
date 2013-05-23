{
  "migrations": [
    {
      "description": "add column blah",
      "pre"  : "select count(*) = 0 from information_schema.columns where table_schema='public' and table_name ='foo' and column_name = 'blah'"
      "up"   : "alter table foo add column blah integer"
      "post" : "select count(*) = 1 from information_schema.columns where table_schema='public' and table_name ='foo' and column_name = 'blah'"
      "down" : "alter table foo drop column blah"
    },
    
  ]
}