program rownaniaNieliniowe
    
    !Kacper Jajuga, kierunek: informatyka, rok I, semestr II

    real a, b, c, h, x, przedzial
    integer n

    write(*,*) 'Podaj POCZATEK przedzialu dla metody przeszukiwan: '
    read(*,*) a
    write(*,*) 'Podaj KONIEC przedzialu dla metody przeszukiwan: '
    read(*,*) b

    while(b .lt. a) do
        write(*,*) 'Koniec przedzialu nie moze byc mniejszy od poczatku przedzialu!'
        write(*,*) 'Ponownie podaj POCZATEK przedzialu dla metody przeszukiwan: '
        read(*,*) a
        write(*,*) 'Ponownie podaj KONIEC przedzialu dla metody przeszukiwan: '
        read(*,*) b
    end while
    
    write(*,*) 'Podaj przyrost h: '
    read(*,*) h

    while(h .le. 0) do
        write(*,*) 'Przyrost nie moze byc mniejszy lub rowny 0!'
        write(*,*) 'Ponownie podaj przyrost h: '
        read(*,*) h
    end while
    
    write(*,*) 'Podaj ostateczna dlugosc przedzialu: '
    read(*,*) przedzial

    while(przedzial .le. 0) do
        write(*,*) 'Ostateczna dlugosc przedzialu nie moze byc mniejsza lub rowna 0!'
        write(*,*) 'Ponownie podaj ostateczna dlugosc przedzialu: '
        read(*,*) h
    end while
    
    while(przedzial .gt. 1) do
        write(*,*) 'Zostala zadeklarowana liczba wieksza od 1, program obliczylby miejsce zerowe ze zbyt mala dokladnoscia'
        write(*,*) 'Ponownie podaj ostateczna dlugosc przedzialu:  '
        read(*,*) h
    end while

    if(f(a)*f(b).lt.0) then
        n=0
        while(abs(b-a).gt.przedzial)do
        write(*,*) 'Przedzial <',a, ', ',b,'>'
            c=(a+b)/2
            if(f(a)*f(c).le.0) then
              b=c
            else
              a=c
            end if
            n=n+1
        end while
        write(*,*) 'Miejsce zerowe to: ', c
        write(*,*) 'f(',c,') = ',f(c)
        write(*,*) 'licznik = ',n
    end if

    write(*,*) 'Nacisnij Enter aby zakonczyc program'
    read(*,*)

end program

    function f(x)
        f=2*x**3-9.06843*x**2-72.3753*x+114.834
    end !f(x)