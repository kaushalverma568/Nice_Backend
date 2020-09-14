INSERT INTO OAUTH_CLIENT_DETAILS(CLIENT_ID, RESOURCE_IDS, CLIENT_SECRET, SCOPE, AUTHORIZED_GRANT_TYPES, 
            WEB_SERVER_REDIRECT_URI, AUTHORITIES, ACCESS_TOKEN_VALIDITY,REFRESH_TOKEN_VALIDITY, ADDITIONAL_INFORMATION, AUTOAPPROVE)
    VALUES ('kody-client', 'resource_id', '$2a$12$ngzfuVDXy2XhNaGtqFlGF.91JsXehU.WChE18HY.yJBeCjtHHe8EW', 'trust,read,user_info,write',
    'refresh_token,password', '', 'ROLE_ADMIN', 30000, 40000, '{"web_server_redirect_uri":"","additional_information":""}', true);

