f = \loc. \loc : x . x+1 ;

g = \loc. \loc : x y z . x+y+z ;

server = spawn ( 
    -- onconnect 하면
    -- (ip, port, "f") 받아서 함수 지정
    -- 서버에서는 "f" 를 가지고 현재 env에 lookup하면
    -- polymorphic location 함수 (\loc. \loc : x . x+1) 나옴
    -- 그것을 클라이언트의 엔트리 식으로 사용
    )