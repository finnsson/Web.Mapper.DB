module Web.Mapper.DB.Sql where


-- | Get all columns for custom types.
--
-- > ID :: Integer      -- ID of type this columm belongs to.
-- > ColName :: String     -- name of column, e.g. ID
-- > ColType :: String  -- name of type of column, e.g. float8
-- > ColNS :: String    -- namespace of type of column, e.g. pg_catalog
-- > Comment :: String  -- comment about this column, e.g. "Primary Key for MyTable"
sqlColumns = " \
 \     select t.oid as ID, a.attname as ColName, \
 \     att.typname as ColType, (select nspname from pg_namespace where oid = att.typnamespace) as ColNS, d.description as Comment \
 \     from pg_type t   \
 \     left join pg_namespace n on t.typnamespace = n.oid   \
 \     left join pg_attribute a on a.attrelid = t.typrelid \
 \     left join pg_type att on att.oid = a.atttypid \
 \     left outer join pg_description d on d.objoid = a.attrelid and d.objsubid = a.attnum  \
 \     where n.nspacl is not null and n.nspname <> 'pg_catalog' and n.nspname <> 'information_schema'  \
 \     and a.attname <> 'cmax' and a.attname <> 'xmax' and a.attname <> 'cmin' and a.attname <> 'xmin'   \ 
 \     and a.attname <> 'ctid'  \
 \     and a.attname <> 'tableoid'  \
 \     and t.typrelid<> 0 \
 \     order by a.attnum; "


-- | Get all custom types (tables/views/compund)
--
-- > ID :: Integer      -- ID of type
-- > Type :: String     -- name of type, e.g. MyTable
-- > TypeNS :: String   -- namespace of type, e.g. public
-- > Comment :: String  -- comment about this type, e.g. "This table rocks!"
sqlTypes = " \
 \     select t.oid as ID, t.typname as TypeName, n.nspname as TypeNS, d.description as Comment, \
 \     has_table_privilege(current_user, c.oid, 'SELECT') as SelectPrivilege, \
 \     has_table_privilege(current_user, c.oid, 'UPDATE') as UpdatePrivilege, \
 \     has_table_privilege(current_user, c.oid, 'INSERT') as InsertPrivilege, \
 \     has_table_privilege(current_user, c.oid, 'DELETE') as DeletePrivilege \
 \     from pg_type t   \
 \     left join pg_namespace n on t.typnamespace = n.oid   \
 \     left join pg_class c on t.typrelid = c.oid \
 \     left join pg_description d on d.objoid = t.typrelid and d.objsubid = 0 \
 \     where n.nspacl is not null and n.nspname <> 'pg_catalog' and n.nspname <> 'information_schema' and t.typrelid<> 0; "



-- get if custom type is table/view and if so if user is permitted to select/insert/delete/update data and update/delete the table.


-- | get all custom functions with description and return-type
sqlFunctions = " \
 \   select p.oid as ID, p.proname as Name, n.nspname as FuncNS, t.typname as Type, tn.nspname as TypeNS, d.description as Comment \
 \   from pg_proc as p                                                         \
 \   join pg_namespace as n on p.pronamespace = n.oid \
 \   join pg_type as t on t.oid = p.prorettype \
 \   join pg_namespace as tn on t.typnamespace = tn.oid \
 \   left join pg_description as d on d.objoid = p.oid \
 \   where n.nspacl is not null and n.nspname <> 'pg_catalog' and n.nspname <> 'information_schema'; "

-- get all custom functions with description and return-type and if user is permitted to execute/update/remove the function.	

-- | Get all arguments for custom functions.
sqlArguments = " \
 \    select n.nspname as FuncNS, p.proname as FuncName, arg.udt_schema as TypeNS, arg.udt_name as TypeName, arg.dtd_identifier as ArgPosition, arg.parameter_name as ArgName \    
 \    from pg_proc as p  \    
 \    join information_schema.parameters as arg \    
 \    on arg.specific_name = p.proname || '_' || p.oid \    
 \    join pg_namespace as n on p.pronamespace = n.oid  \    
 \    where n.nspacl is not null and n.nspname <> 'pg_catalog' and n.nspname <> 'information_schema' ;"


pgProcSql = "                                                                              \
         \      select                                                                       \
         \              p.proname as Name,                                                   \
         \              n.nspname as NamespaceName,                                          \
         \              oidvectortypes(p.proargtypes) as ArgumentTypes,                      \ 
         \              (select typname from pg_type where oid = p.prorettype) as ReturnType \
         \      from                                                                         \
         \              pg_proc as p                                                         \
         \      join                                                                         \
         \              pg_namespace as n                                                    \
         \      on                                                                           \
         \              p.pronamespace = n.oid                                               \
         \      where                                                                        \
         \              n.nspacl is not null and                                             \
         \              n.nspname <> 'pg_catalog' and n.nspname <> 'information_schema'      "

pgTypeSql = "                                       \
         \     select t.oid, c.relkind, c.relname     \
         \     from pg_type t                         \
         \     left join pg_namespace n               \
         \     on t.typnamespace = n.oid              \
         \     left join pg_class c                   \
         \     on t.typrelid = c.oid                  \
         \     left join pg_description d             \
         \     on d.classoid = c.oid                  \
         \     where n.nspname <> 'pg_catalog'        \
         \     and n.nspacl is not null               \
         \     and n.nspname <> 'information_schema'  \
         \     and t.typrelid<> 0;                    "

pgProcParameter = "select * from information_schema.parameters where specific_schema = 'public';"

pgTableSql = "        \
   \  select t.oid, a.attname, t.typname, n.nspname, \
   \  (select typname from pg_type where oid = a.atttypid) as ArgumentType, \ 
   \  (select nspname from pg_namespace where oid = (select typnamespace from pg_type where oid = a.atttypid)) as ArgumentNamespace \ 
   \  from pg_type t \ 
   \  left join pg_namespace n \ 
   \  on t.typnamespace = n.oid \ 
   \  left join pg_class c \ 
   \  on t.typrelid = c.oid \ 
   \  left join pg_description d \ 
   \  on d.classoid = c.oid \ 
   \  left join pg_attribute a \ 
   \  on a.attrelid = c.oid \ 
   \  where n.nspacl is not null and n.nspname <> 'pg_catalog' and n.nspname <> 'information_schema' \
   \  and a.attname <> 'cmax' and a.attname <> 'xmax' and a.attname <> 'cmin' and a.attname <> 'xmin'  \ 
   \  and a.attname <> 'ctid' \
   \  and a.attname <> 'tableoid' \
   \  and t.typrelid<> 0;"


