asect 0x00


table:     # each triplet below represents a line of three cells
dc 1,2,3   # horizontal lines
dc 5,6,7   #
dc 9,10,11 #
dc 1,5,9   # vertical lines
dc 2,6,10  #
dc 3,7,11  #
dc 1,6,11  # diagonal lines
dc 3,6,9   #

ldi r0, table # Загружаем таблицу и счётчик
ldi r1, 24

initNext:
ldc r0, r2 # Загружаем из таблицы
st r0, r2 # данные в ОЗУ
inc r0
dec r1
bnz initNext

#ldi r0, 0xf3
#ldi r1, 0b10000101
#st r0, r1

readbutton:
ldi r0, 0xf3 # Load the button id in r0
while
ld r0, r1
tst r1
stays pl
wend

ldi r2, 0b01111111
and r2, r1
inc r1

ldi r2, 24 #счёtчик
ldi r0, table #обращение к tаблице

obnulenie:
ld r0, r3
if
cmp r3, r1
is eq
ldi r3, 0
st r0, r3
fi
inc r0
dec r2 #обнуление счётчика
bnz obnulenie

dec r1 #делаем крестик
shla r1
shla r1
inc r1
inc r1
ldi r0, 0xf3
st r0, r1


ldi r2, 8 #счёtчик
ldi r0, table #обращение к tаблице

win:
ld r0, r1 #1
inc r0
ld r0, r3 #2
inc r0
add r1, r3
ld r0, r1 #3
if
add r1, r3
is z
ldi r0,0xf3 #итог - игрок победил
ldi r1, 0b01111111 #значит полученное значение пойдёт на шину I/Odat, а после в разветвитель и в чип
st r0, r1 #выводим победу
halt #конец игре
fi
inc r0
dec r2
bnz win

ldi r0, table
ldi r2, 9
ldi r3, 0

draw:
ld r0, r1
add r1, r3
inc r0
dec r2
bnz draw
if
tst r3
is mi
ldi r0,0xf3 #итог - никто не победил
ldi r1, 0b11111111 #значит полученное значение пойдёт на шину I/Odat, а после в разветвитель и в чип
st r0, r1 #выводим ничью
halt #конец игре
fi

#берём значение из стека, чтобы понять где один из крестиков
###################################################################
Ai:

Centr:
ldi r0, table
ldi r2, 4
add r2, r0
if
ld r0, r2
is eq #значит крестик посередине
ldi r0, table
if
ld r0, r2
is gt
ldi r3, 1
br nachalo
fi
fi
if 
tst r2
is gt #посередине нет крестика
ldi r3, 6
br nachalo
fi

ldi r0, table
ldi r2, 8
Deffence:
ld r0, r1
inc r0
ld r0, r3

if
cmp r1, r3
is eq
inc r0
ld r0, r3
if
tst r3
is gt
br nachalo
dec r0
fi
fi

inc r0
ld r0, r3

if
cmp r1, r3
is eq
dec r0
ld r0, r3
if
tst r3
is gt
br nachalo
fi
inc r0
fi

dec r0
ld r0, r1

if
cmp r1, r3
is eq
dec r0
ld r0, r3
if
tst r3
is gt
br nachalo
fi
inc r0
fi
inc r0
inc r0
dec r2
bnz Deffence

Random:
ldi r2, 0
ldi r1, -1
while #начинается цикл по поиску свободной ячейки (а точнее ячейки не равной 0 или -1)
tst r2
stays eq
ld r0, r3
if
cmp r2, r3 #ячейка не заполнена 0 (крестик)
is ne
if
cmp r1, r3 #ячейка не заполнена -1 (нолик)
is ne
inc r2
fi
fi
inc r0
wend


nachalo:

ldi r0, table #обращение к tаблице
ldi r2, 24 #счёtчик

otritsanie:
ld r0, r1
if
cmp r3, r1
is eq
ldi r1, -1
st r0, r1
fi
inc r0
dec r2 #обнуление счётчика
bnz otritsanie

dec r3
shla r3 #получаем нолик
shla r3
inc r3
ldi r0, 0xf3 #выводим нолик в игровое поле
st r0, r3

ldi r2, 8 #счёtчик
ldi r0, table #обращение к tаблице

lose:
ld r0, r1 #1
inc r0
ld r0, r3 #2
inc r0
add r1, r3
ld r0, r1 #3
add r1, r3
ldi r1, -3
if
cmp r3, r1
is eq
ldi r0,0xf3 #итог - игрок проиграл
ldi r1, 0b10111111 #значит полученное значение пойдёт на шину I/Odat, а после в разветвитель и в чип
st r0, r1 #выводим проигрыш
halt #конец игре
fi
inc r0
dec r2
bnz lose

last:
br readbutton

end 