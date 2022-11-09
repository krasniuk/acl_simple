 #! /bin/bash


echo "show_allow_roles -> " > file_test
 
curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"method":"show_allow_roles"}' \
    http://localhost:8080 >> file_test
    
echo "" >> file_test
echo "=========" >> file_test





#==============

echo "show_all_users -> " >> file_test
 
curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"method":"show_all_users"}' \
    http://localhost:8080 >> file_test

echo "" >> file_test
echo "-------" >> file_test

#---------------

echo "user_add(mike_test) -> " >> file_test
 
curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"method":"user_add", "user":"mike_test"}' \
    http://localhost:8080 >> file_test
    
echo "" >> file_test
echo "-------" >> file_test
#---------------

echo "user_add(karl_test) -> " >> file_test
 
curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"method":"user_add", "user":"karl_test"}' \
    http://localhost:8080 >> file_test
    
echo "" >> file_test
echo "-------" >> file_test

#---------------

echo "show_all_users -> " >> file_test
 
curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"method":"show_all_users"}' \
    http://localhost:8080 >> file_test

echo "" >> file_test
echo "-------" >> file_test

#---------------

echo "roles_add(r) -> " >> file_test
 
curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"method":"roles_add", "user":"karl_test", "roles":["read"]}' \
    http://localhost:8080 >> file_test
    
echo "" >> file_test
echo "-------" >> file_test

#---------------

echo "roles_add(r, w) -> " >> file_test
 
curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"method":"roles_add", "user":"karl_test", "roles":["read", "write"]}' \
    http://localhost:8080 >> file_test
    
echo "" >> file_test
echo "-------" >> file_test

#---------------

echo "show_roles(karl_test) -> " >> file_test
 
curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"method":"show_roles", "user":"karl_test"}' \
    http://localhost:8080 >> file_test
    
echo "" >> file_test
echo "-------" >> file_test

#---------------

echo "roles_delete(e) -> " >> file_test
 
curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"method":"roles_delete", "user":"karl_test", "roles":["exec"]}' \
    http://localhost:8080 >> file_test
    
echo "" >> file_test
echo "-------" >> file_test

#---------------

echo "roles_delete(e, r) -> " >> file_test
 
curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"method":"roles_delete", "user":"karl_test", "roles":["exec", "read"]}' \
    http://localhost:8080 >> file_test
    
echo "" >> file_test
echo "-------" >> file_test

#---------------

echo "show_roles(karl_test) -> " >> file_test
 
curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"method":"show_roles", "user":"karl_test"}' \
    http://localhost:8080 >> file_test
    
echo "" >> file_test
echo "-------" >> file_test

#---------------

echo "roles_delete(w) -> " >> file_test
 
curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"method":"roles_delete", "user":"karl_test", "roles":["write"]}' \
    http://localhost:8080 >> file_test
    
echo "" >> file_test
echo "-------" >> file_test

#---------------

echo "show_roles(karl_test) -> " >> file_test
 
curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"method":"show_roles", "user":"karl_test"}' \
    http://localhost:8080 >> file_test
    
echo "" >> file_test
echo "-------" >> file_test

#---------------

echo "roles_add(r, w) -> " >> file_test
 
curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"method":"roles_add", "user":"karl_test", "roles":["read", "write"]}' \
    http://localhost:8080 >> file_test
    
echo "" >> file_test
echo "-------" >> file_test

#---------------

echo "user_delete(mike_test) -> " >> file_test
 
curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"method":"user_delete", "user":"mike_test"}' \
    http://localhost:8080 >> file_test
    
echo "" >> file_test
echo "-------" >> file_test

#---------------

echo "show_all_users -> " >> file_test
 
curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"method":"show_all_users"}' \
    http://localhost:8080 >> file_test
    
echo "" >> file_test
echo "-------" >> file_test

#---------------

echo "user_delete(karl_test) -> " >> file_test
 
curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"method":"user_delete", "user":"karl_test"}' \
    http://localhost:8080 >> file_test
    
echo "" >> file_test
echo "-------" >> file_test

#---------------

echo "show_all_users -> " >> file_test
 
curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"method":"show_all_users"}' \
    http://localhost:8080 >> file_test
    
echo "" >> file_test


cat file_test

rm file_test
