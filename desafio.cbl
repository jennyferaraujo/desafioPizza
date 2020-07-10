       identification division.
       program-id. "desafio".
       author. "Jennyfer Araujo".
       installation. "PC".
       date-written. 09/07/2020.
       date-compiled. 09/07/2020.

       environment division.
       configuration section.
           special-names. decimal-point is comma.

       input-output section.
       file-control.
       i-o-control.

       data division.

       file section.

       working-storage section.

       01 relatorio occurs 20.
          05 nome                                 pic x(15)
                                                  value spaces.
          05 filler                               pic x(03)
                                                  value "-".
          05 diametro                             pic 9(03).
          05 filler                               pic x(03)
                                                  value "-".
          05 preco                                pic 9(03)v99.
          05 filler                               pic x(03)
                                                  value "-".
          05 preco_cm2                            pic 9(03)v99.
          05 filler                               pic x(03)
                                                  value "-".
          05 diferenca_rel                        pic 9(03)v99.

       77 ind                                     pic 9(02).
       77 menu                                    pic x(01).
       77 raio                                    pic 9(03)v99.
       77 area_pizza                              pic 9(03)v99.
       77 controle                                pic x(10).
       77 aux                                     pic 9(03)v99.
       77 delta_preco_cm2                         pic 9(03)v99.

       linkage section.

       screen section.

       procedure division.

           display "---- Custo Beneficio Pizza ----"

           perform inicializa.
           perform processamento.
           perform finaliza.

       inicializa section.

           move   "S"   to    menu
           .

       inicializa-exit.
           exit.

       processamento section.

           move 0 to ind
           perform until menu <> "S"
               display erase
               add 1 to ind

           if ind > 20 then
               display "Voce atingiu o limite de 20 pizzas"
           else
               display "Informe o nome da pizza: "
               accept nome(ind)

               display "Informe o diametro: "
               accept diametro(ind)

               display "Informe o preco: "
               accept preco(ind)

           end-if

               perform preco-cm2

           display "Deseja cadastrar mais uma pizza? ('S'/'N')"
           accept menu

           end-perform

           perform ordenar
           perform calculo-porcent

           perform varying ind from 1 by 1 until ind > 20 or
                                            nome(ind) = space

           display relatorio(ind)

           end-perform

           .
       processamento-exit.
           exit.


      *---------------

       preco-cm2 section.

           compute raio = diametro(ind) / 2
           compute area_pizza = (3,14 * (raio * raio))
           compute preco_cm2(ind) = preco(ind) / area_pizza
           .

       preco-cm2-exit.
           exit.

      *---------------

       ordenar section.

           move    "trocou"    to    controle
           perform until controle <> "trocou"

           move 1 to ind
           move "N_trocou" to controle

           perform until ind = 20
                      or nome(ind + 1) = space

               if preco_cm2(ind) > preco_cm2(ind + 1) then
                   move preco_cm2 (ind + 1) to aux
                   move preco_cm2(ind)      to preco_cm2(ind + 1)
                   move aux                 to preco_cm2(ind)

                   move "trocou"            to controle

               end-if

               add 1 to ind

               end-perform

           end-perform
           .

       ordenar-exit.
           exit.

      *---------------

       calculo-porcent section.
           move    1    to    ind

           perform until ind = 20
                      or nome(ind + 1) = spaces

           compute delta_preco_cm2 = preco_cm2(ind + 1) -
                                     preco_cm2(ind)

           compute diferenca_rel(ind + 1) = (delta_preco_cm2 * 100)
                                             / preco_cm2(ind)

               add 1 to ind

           end-perform
           .

       calculo-porcent-exit.
           exit.

      *-----------------

       finaliza section.
           stop run
           .
       finaliza-exit.
           exit.
