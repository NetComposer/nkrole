# Introduction

NkROLE is a framework for managing complex relations among arbitrary objects in a [_riak_core_](https://github.com/basho/riak_core) cluster. You can create any number of objects (Erlang terms) and define relations among them, based on _roles_. Any object can be said to _have_ any _role_ over any other object. Also, you can say that all objects having a specific role over an object have automatically the same or other role over other object, and this complex relations can be nested at any level.

NkROLE creates a number of caches, so that, once all objects having a role over another object are found, the list is saved for future queries. If any of the base conditions used for the calculation change (because the base object or any of the objects used for the calculation have changed) all related caches are automatically invalidated (but only the related caches).

NkROLE creates a process for each used object in the cluster (using [_NkDIST_](https://github.com/nekso/nkdist)), called the _object proxy_. Object proxies are spread evenly in the cluster, and have a timeout value. If no further query is received for a period of time, they are removed.

For each query that involves nested roles, a cache (another Erlang process) is started for each specific role at each related proxy, also with an specific timeout value, so that new queries are very fast, even with a huge number of objects and nested relations.

NkROLE scales automatically with the number of nodes, spreading the node proxies and caches evenly in the cluster. When new nodes are added or removed, the cluster adapts automatically. If necesary, proxy objects are moved, but caches are not moved. If the should go to another node, they are deleted and recreated there when needed.

NkROLE can use any external backend for the object storage, implementing the [nkrole_store](src/nkrole_store.erl) behaviour and using the `backend` configuration value. Of course the backend must be accesible from all nodes. Two demonstration backends are included:

* ETS: very fast but not distributed, so it can be used only in 1-node tests.
* Riak Core Metadata: distributed but slow for many objects.


# Example1

Let's say we want to model a _member_ relationship, following this schema:

![Roles1](test/Roles1.png)
We want to say that:

* _Members_ of object `root` are all objects being _members_ of `orgA` or `orgB`.
* _Members_ of `orgA` are all objects members of `depA1`, `depA2` or `depA3`.
* _Member_ of `depA1` are `u01` and `u02`.
* _Members_ of `depA2` are `depA21`, `depA22` and also all object being members of thouse objects.
* ... and so on.

We can create this structure:
```
$ make shell
```

```erlang
> nkrole_store:put_obj(root, #{member=>[{member, orgA}, {member, orgB}}], #{}).
ok

> nkrole_store:put_obj(orgA, #{member=>[{member, depA1}, {member, depA2}, {member, depA3}], #{}).
ok

> nkrole_store:put_obj(orgB, #{member=>[u10, {member, depB1}], #{}).
ok
```

and so on, or you could first create the objects and then apply the roles:
```erlang
> nkrole:put_obj(root, #{}, #{}).
ok

> nkrole_proxy:add_subrole(member, root, member, orgA, #{}).
ok
```

This particular structure happens to be used at the tests, so you can create all at once:

```
$ make build_tests
$ make shell
```

```erlang
> test_util:insert(set1).
ok
```

Now we can query in many ways:
```erlang
> % Get direct roles
> nkrole_proxy:get_role_objs(orgB, #{}).
{ok, [u10, {member, depB1}]}

> % Get nested roles
> nkrole_proxy:find_role_objs(orgB, #{}).
{ok, [u10, u11, 12]}

> nkrole_proxy:find_role_objs(root, #{}).
{ok,[u01,u02,depA21,depA22,u03,u04,u05,u06,u07,u08,u10,u11,u12]}.

> % Check users
> nkrole_proxy:has_role(u03, member, depA21, #{}).
{ok, true}

> nkrole_proxy:has_role(u03, member, depB1, #{}).
{ok, false}

> nkrole_proxy:has_role(u03, member, root, #{}).
{ok, true}
```


# Example2

The previous test data adds also another role called `head` to the same schema:

![Roles1](test/Roles2.png)

We want to say that all users report to their department (except `u10`), departments to their organizations, and organizations to root:

```
$ make build_tests
$ make shell
```

```erlang
> test_util:insert(set1).
ok
```

Now we can query:
```erlang
> nkrole_proxy:get_role_objs(head, u05, #{}).
{ok,[depA22,{head,depA22}]}

> nkrole_proxy:find_role_objs(head, u05, #{}).
{ok,[depA22, depA2, orgA, root]}
```

This second example is very similar to the case for nested configuration, where an object is configured based on one or more _parents_.


# Configuration

NkROLE uses standard Erlang application environment variables. The same Erlang application is used for agents and controllers. 

Option|Type|Default|Desc
---|---|---|---
backend|`ets|riak_core|atom()`|`ets`|Backend to use, implementing the nkrole_store behaviour
proxy_timeout|`pos_integer()`|`180000`|Timeout for proxy objects
cache_timeout|`pos_integer()`|`180000`|Timeout for cache objects
