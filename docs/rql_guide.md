# RQL - Resource Query Language

Resource Query Language (RQL) is a query language designed for use in URIs with object-style data structures. DelphiMVCFramework supports RQL natively, and the included MVCActiveRecord framework implements a large subset of the RQL specifications.

## Overview

RQL can be thought of as a set of nestable named operators, each with a set of arguments. RQL is designed to have an extremely simple but extensible grammar that can be written in a URL-friendly query string.

### Basic Example

A simple RQL query with a single operator that searches for resources with a property "name" equal to "John":

```rql
eq(name,John)
```

### Complex Example

A more complex filter with multiple chained functions:

```rql
or(and(eq(name,John),eq(surname,Doe)),and(eq(name,Jane),eq(surname,Smith)));sort(+name);limit(10,0)
```

This translates to SQL similar to:

```sql
SELECT * FROM table 
WHERE (name = 'John' AND surname = 'Doe') 
   OR (name = 'Jane' AND surname = 'Smith')
ORDER BY name ASC
LIMIT 10 OFFSET 0
```

## Supported Operators

### Comparison Operators

#### `eq(property, value)` - Equals
Filters for objects where the specified property's value equals the provided value.

**Examples**:
```rql
eq(name,John)           -- name = 'John'
eq(age,25)              -- age = 25
eq(active,true)         -- active = true
eq(salary,50000.50)     -- salary = 50000.50
```

#### `ne(property, value)` - Not Equals
Filters for objects where the specified property's value is not equal to the provided value.

**Examples**:
```rql
ne(status,inactive)     -- status <> 'inactive'
ne(age,0)               -- age <> 0
```

#### `lt(property, value)` - Less Than
Filters for objects where the specified property's value is less than the provided value.

**Examples**:
```rql
lt(age,30)              -- age < 30
lt(price,100.00)        -- price < 100.00
lt(created_date,2023-01-01)  -- created_date < '2023-01-01'
```

#### `le(property, value)` - Less Than or Equal
Filters for objects where the specified property's value is less than or equal to the provided value.

**Examples**:
```rql
le(age,65)              -- age <= 65
le(discount,0.15)       -- discount <= 0.15
```

#### `gt(property, value)` - Greater Than
Filters for objects where the specified property's value is greater than the provided value.

**Examples**:
```rql
gt(salary,50000)        -- salary > 50000
gt(rating,4.0)          -- rating > 4.0
```

#### `ge(property, value)` - Greater Than or Equal
Filters for objects where the specified property's value is greater than or equal to the provided value.

**Examples**:
```rql
ge(age,18)              -- age >= 18
ge(score,80)            -- score >= 80
```

### Logical Operators

#### `and(query1, query2, ...)` - Logical AND
Combines multiple queries with AND logic. All conditions must be true.

**Examples**:
```rql
and(eq(category,electronics),gt(price,100))
-- category = 'electronics' AND price > 100

and(eq(active,true),ge(age,18),le(age,65))
-- active = true AND age >= 18 AND age <= 65
```

#### `or(query1, query2, ...)` - Logical OR
Combines multiple queries with OR logic. At least one condition must be true.

**Examples**:
```rql
or(eq(category,books),eq(category,magazines))
-- category = 'books' OR category = 'magazines'

or(eq(priority,high),eq(urgent,true))
-- priority = 'high' OR urgent = true
```

### Array Operators

#### `in(property, array-of-values)` - IN Operator
Filters for objects where the specified property's value is in the provided array.

**Examples**:
```rql
in(status,(active,pending,review))
-- status IN ('active', 'pending', 'review')

in(category_id,(1,2,3,4,5))
-- category_id IN (1, 2, 3, 4, 5)

in(country,(US,CA,MX))
-- country IN ('US', 'CA', 'MX')
```

#### `out(property, array-of-values)` - NOT IN Operator
Filters for objects where the specified property's value is NOT in the provided array.

**Examples**:
```rql
out(status,(deleted,archived))
-- status NOT IN ('deleted', 'archived')

out(user_id,(123,456,789))
-- user_id NOT IN (123, 456, 789)
```

#### `contains(property, value)` - Array Contains
Filters for objects where the specified property's value is an array and contains the provided value.

**Examples**:
```rql
contains(tags,javascript)
-- 'javascript' IN tags (where tags is an array)

contains(permissions,admin)
-- 'admin' IN permissions
```

### Sorting

#### `sort(direction+property, ...)` - Sort Results
Sorts results by the specified properties. Use `+` for ascending, `-` for descending.

**Examples**:
```rql
sort(+name)             -- ORDER BY name ASC
sort(-created_date)     -- ORDER BY created_date DESC
sort(+category,-price)  -- ORDER BY category ASC, price DESC
sort(-priority,+name)   -- ORDER BY priority DESC, name ASC
```

### Pagination

#### `limit(count, start, maxCount)` - Limit Results
Returns a specific range of objects from the result set.

**Parameters**:
- `count`: Number of records to return
- `start`: Starting offset (0-based)
- `maxCount`: Maximum allowed count (optional, server-enforced)

**Examples**:
```rql
limit(10)               -- LIMIT 10
limit(20,0)             -- LIMIT 20 OFFSET 0 (first page)
limit(20,20)            -- LIMIT 20 OFFSET 20 (second page)
limit(50,100,1000)      -- LIMIT 50 OFFSET 100 (max 1000)
```

## Using RQL with MVCActiveRecord

### Basic Query Example

```pascal
// Simple equality
var Customers := TMVCActiveRecord.SelectRQL<TCustomer>('eq(city,Rome)', 50);

// Multiple conditions
var Products := TMVCActiveRecord.SelectRQL<TProduct>(
  'and(eq(category,electronics),gt(price,100))', 20);

// With sorting
var Users := TMVCActiveRecord.SelectRQL<TUser>(
  'and(eq(active,true),ge(age,18));sort(+lastname,+firstname)', 100);
```

### Advanced Query Examples

```pascal
// Complex filtering with pagination
var Orders := TMVCActiveRecord.SelectRQL<TOrder>(
  'and(ge(order_date,2023-01-01),le(order_date,2023-12-31),in(status,(completed,shipped)));sort(-order_date);limit(25,0)', 25);

// Search across multiple fields
var Employees := TMVCActiveRecord.SelectRQL<TEmployee>(
  'or(contains(skills,java),contains(skills,pascal),contains(skills,python));sort(+hire_date)', 50);

// Nested logical conditions
var Reports := TMVCActiveRecord.SelectRQL<TReport>(
  'and(or(eq(type,monthly),eq(type,quarterly)),and(ge(year,2020),eq(published,true)));sort(-year,-month)', 100);
```

### Count Records with RQL

```pascal
// Count with conditions
var CustomerCount := TMVCActiveRecord.CountRQL<TCustomer>('eq(country,Italy)');

// Count with complex conditions
var ActiveUserCount := TMVCActiveRecord.CountRQL<TUser>(
  'and(eq(active,true),ge(last_login,2023-01-01))');
```

### Delete with RQL

```pascal
// Delete inactive users older than 2 years
var DeletedCount := TMVCActiveRecord.DeleteRQL<TUser>(
  'and(eq(active,false),lt(last_login,2022-01-01))');

// Delete completed orders older than 5 years
var DeletedOrders := TMVCActiveRecord.DeleteRQL<TOrder>(
  'and(eq(status,completed),lt(order_date,2019-01-01))');
```

## RQL in REST Controllers

### URL Query Parameters

RQL can be passed as URL query parameters:

```
GET /api/customers?filter=eq(city,Rome)
GET /api/products?filter=and(eq(category,books),gt(price,20))&sort=-price
GET /api/users?filter=contains(roles,admin)&limit=10,0
```

### Controller Implementation

```pascal
[MVCPath('/api/customers')]
TCustomersController = class(TMVCController)
public
  [MVCPath('/')]
  [MVCHTTPMethod([httpGET])]
  procedure GetCustomers;
end;

procedure TCustomersController.GetCustomers;
var
  RQLFilter: string;
  Customers: TObjectList<TCustomer>;
begin
  RQLFilter := Context.Request.Params['filter'];
  if RQLFilter.IsEmpty then
    RQLFilter := 'eq(active,true)'; // Default filter
    
  Customers := TMVCActiveRecord.SelectRQL<TCustomer>(RQLFilter, 100);
  try
    Render(Customers);
  finally
    Customers.Free;
  end;
end;
```

### Advanced Controller with Pagination

```pascal
procedure TCustomersController.GetCustomers;
var
  RQLFilter: string;
  Page, PageSize: Integer;
  Customers: TObjectList<TCustomer>;
  TotalCount: Int64;
begin
  // Get parameters
  RQLFilter := Context.Request.Params['filter'];
  Page := StrToIntDef(Context.Request.Params['page'], 1);
  PageSize := StrToIntDef(Context.Request.Params['pagesize'], 20);
  
  // Build RQL with pagination
  if not RQLFilter.IsEmpty then
    RQLFilter := RQLFilter + ';'
  else
    RQLFilter := '';
    
  RQLFilter := RQLFilter + Format('limit(%d,%d)', [PageSize, (Page-1) * PageSize]);
  
  // Execute query
  Customers := TMVCActiveRecord.SelectRQL<TCustomer>(RQLFilter, PageSize);
  TotalCount := TMVCActiveRecord.CountRQL<TCustomer>(
    Context.Request.Params['filter']); // Count without limit
  
  try
    // Return with metadata
    Render(ObjectDict()
      .Add('data', Customers)
      .Add('pagination', StrDict(['page', 'pagesize', 'total'], 
                                [Page.ToString, PageSize.ToString, TotalCount.ToString])));
  finally
    Customers.Free;
  end;
end;
```

## Named Queries

MVCActiveRecord supports named RQL queries for better maintainability and performance.

### Defining Named Queries

```pascal
[MVCNamedRQLQuery('ActiveCustomers', 'and(eq(active,true),ge(registration_date,%s))')]
[MVCNamedRQLQuery('CustomersByCity', 'eq(city,%s)', TMVCActiveRecordBackEnd.PostgreSQL)]
[MVCNamedRQLQuery('CustomersByCity', 'city = %s', TMVCActiveRecordBackEnd.SQLite)]
TCustomer = class(TMVCActiveRecord)
// ... entity definition
end;
```

### Using Named Queries

```pascal
// Use named RQL query
var RecentCustomers := TMVCActiveRecord.SelectRQLByNamedQuery<TCustomer>(
  'ActiveCustomers', ['2023-01-01'], 50);

// With multiple parameters
var CityCustomers := TMVCActiveRecord.SelectRQLByNamedQuery<TCustomer>(
  'CustomersByCity', ['Rome'], 100);

// Count with named query
var Count := TMVCActiveRecord.CountRQLByNamedQuery<TCustomer>(
  'ActiveCustomers', ['2023-01-01']);
```

## Database-Specific Features

### PostgreSQL Extensions

```pascal
// JSON field queries (PostgreSQL only)
var ProductsWithSpecs := TMVCActiveRecord.SelectRQL<TProduct>(
  'contains(specifications.colors,red)', 20);

// Array field operations
var UsersWithRole := TMVCActiveRecord.SelectRQL<TUser>(
  'contains(roles,admin)', 50);
```

### SQLite Adaptations

```pascal
// SQLite automatically handles date/time conversions
var RecentOrders := TMVCActiveRecord.SelectRQL<TOrder>(
  'ge(order_date,2023-01-01)', 100);
```

## Performance Considerations

### Indexing

Ensure database indexes exist for frequently queried fields:

```sql
-- Index for common RQL queries
CREATE INDEX idx_customers_city ON customers(city);
CREATE INDEX idx_products_category_price ON products(category, price);
CREATE INDEX idx_orders_date_status ON orders(order_date, status);
```

### Query Optimization

```pascal
// Use specific fields instead of generic queries
// Good:
eq(status,active)

// Less efficient:
contains(description,status)

// Combine conditions efficiently
// Good:
and(eq(active,true),eq(country,US))

// Less efficient:
and(ne(active,false),in(country,(US)))
```

### Caching Results

```pascal
// Cache frequently used queries
var CachedResults := GetCachedRQLResults('popular_products');
if not Assigned(CachedResults) then
begin
  CachedResults := TMVCActiveRecord.SelectRQL<TProduct>(
    'and(eq(featured,true),gt(rating,4.0));sort(-sales_count)', 10);
  SetCachedRQLResults('popular_products', CachedResults, 300); // 5 min cache
end;
```

## Error Handling

### Common RQL Errors

```pascal
try
  var Results := TMVCActiveRecord.SelectRQL<TCustomer>('invalid_syntax', 10);
except
  on E: EMVCActiveRecordRQLSyntaxError do
  begin
    // Handle syntax error
    LogError('RQL Syntax Error: ' + E.Message);
    // Return appropriate HTTP error
    raise EMVCException.Create(HTTP_STATUS.BadRequest, 
      'Invalid query syntax: ' + E.Message);
  end;
  on E: EMVCActiveRecordFieldNotFound do
  begin
    // Handle unknown field error
    LogError('RQL Field Error: ' + E.Message);
    raise EMVCException.Create(HTTP_STATUS.BadRequest, 
      'Unknown field in query: ' + E.Message);
  end;
end;
```

### Validation

```pascal
function ValidateRQLQuery(const RQL: string; const EntityClass: TMVCActiveRecordClass): Boolean;
begin
  try
    // Try to parse without executing
    var Parser := TMVCRQLParser.Create;
    try
      Parser.Parse(RQL, EntityClass);
      Result := True;
    finally
      Parser.Free;
    end;
  except
    Result := False;
  end;
end;
```

## Best Practices

### URL Encoding
Always URL-encode RQL queries when passing in URLs:

```
// Raw RQL
and(eq(name,John Doe),gt(age,25))

// URL encoded
and(eq(name,John%20Doe),gt(age,25))
```

### Security Considerations

```pascal
// Validate and sanitize user input
function SanitizeRQLInput(const UserRQL: string): string;
begin
  // Remove potentially dangerous characters
  Result := StringReplace(UserRQL, ';DROP', '', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, 'DELETE', '', [rfReplaceAll, rfIgnoreCase]);
  // Add more validation as needed
end;
```

### Documentation

Document your RQL endpoints:

```pascal
[MVCDoc('Get customers with filtering support')]
[MVCSwagSummary('Customer List', 'Returns filtered list of customers')]
[MVCSwagParam(plQuery, 'filter', 'RQL filter expression', 
  'eq(city,Rome) or and(gt(age,25),eq(active,true))')]
[MVCSwagParam(plQuery, 'sort', 'Sort expression', '+name or -created_date')]
[MVCSwagParam(plQuery, 'limit', 'Pagination', '20,0 (pagesize,offset)')]
procedure GetCustomers;
```

## Migration from SQL

### Common SQL to RQL Mappings

| SQL | RQL |
|-----|-----|
| `WHERE name = 'John'` | `eq(name,John)` |
| `WHERE age > 25` | `gt(age,25)` |
| `WHERE city IN ('Rome', 'Milan')` | `in(city,(Rome,Milan))` |
| `WHERE active = true AND age >= 18` | `and(eq(active,true),ge(age,18))` |
| `WHERE name LIKE '%john%'` | `contains(name,john)` |
| `ORDER BY name ASC` | `sort(+name)` |
| `ORDER BY age DESC, name ASC` | `sort(-age,+name)` |
| `LIMIT 20 OFFSET 40` | `limit(20,40)` |

---

RQL provides a powerful, URL-friendly way to query your data with MVCActiveRecord. Its simple syntax makes it easy for API consumers while providing the flexibility needed for complex queries.