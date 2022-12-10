request = function()
    path = "/"
    wrk.headers["Content-Type"] = "application/json; charset=utf-8"
    wrk.body = '{"method":"show_all_users"}'
    return wrk.format("POST", path)
end

response = function(status, headers, body)
    io.write('----------------------------\n')
    io.write('status = ' .. status ..'\n')
    io.write('body = '.. body ..'\n')
end