
(*
Write an SML program (users.sml) that computes the permissions for all the users in a course (students, graduate TAs, undergraduate Tas, graders and instructors). This is called the role-based access control (RBAC) problem. It is used in operating systems (user, groups, permissions to folders and files), databases, etc.

We consider the following sets:

USERS set of users
ROLES set of roles
PERMS set of permissions
UR set of user-role pairs is subset of USERS * ROLES
RP set of role-permission pairs is subset of ROLES * PERMS
RR set of pairs of roles, called ascendant and descendant roles, where an ascendant role inherits all permissions from a descendant role. This role hierarchy is transitive.

 Write a function authorizedRoles(User, ListUserRoleTuples, ListRoleRoleTuples) that returns the set UserAuthorizedRoles (represented as a list) of roles that user has.

 Write a function authorizedPermissions(User, UserAuthorizedRoles, ListRolePermissionTuples) that returns the set UserAuthorizedPermissions (represented as a list) of permissions that user has.
*)

fun member(x,[]) = false
| member(x,Y::Ys) = 
    if(x=Y) then true
    else member(x,Ys);
    
fun union([],L2) = L2
|union(x::xs,L2) = 
    if member(x,L2) then union(xs,L2)
    else x::union(xs,L2);

fun roleHierarchyHelper(role,[]) = [] 
| roleHierarchyHelper(role,(x,y)::RR) = 
    if(role = x) 
        then y::roleHierarchyHelper(role,RR)
    else
        roleHierarchyHelper(role,RR);
        
fun roleHierarchy([],RR) = []
| roleHierarchy(role::roles,RR) = 
    union(role::roleHierarchy(roleHierarchyHelper(role,RR),RR),roleHierarchy(roles,RR));


fun findRoles(user,[]) = []
| findRoles(user,(x,y)::UR) = 
    if(user = x) then y::findRoles(user,UR)
    else
        findRoles(user,UR);

fun authorizedRoles(user, UR,RR) = 
    if UR = [] then [] 
    else 
    roleHierarchy(findRoles(user,UR),RR);



fun authorizedPermissions(User, [], RolePermissionTuples) = []
| authorizedPermissions(User,role::roles,RolePermissionTuples) = 
    union(roleHierarchyHelper(role,RolePermissionTuples),authorizedPermissions(User,roles,RolePermissionTuples));