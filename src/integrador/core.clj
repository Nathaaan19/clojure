(ns integrador.core)

(def NOTA-MINIMA 6.0)

(defn adicionar-status
  "Recebe um mapa de aluno e retorna o mapa com a chave :status adicionada."
  [aluno]
  (assoc aluno :status (if (>= (:nota aluno) NOTA-MINIMA)
                         "Aprovado"
                         "Reprovado")))

(defn calcular-media
  "Calcula a media das notas de uma lista de alunos."
  [alunos]
  (if (empty? alunos)
    0.0 
    (let [soma-notas (reduce + (map :nota alunos))] 
      (/ soma-notas (count alunos)))))


(defn cadastrar-alunos
  "Implementa a Opção 1: Cadastra alunos em loop."
  [alunos-atuais]
  (println "\nCadastro de Alunos")
  (loop [novos-alunos []] 
    (println "Digite o nome do aluno (ou deixe em branco para parar):")
    (let [nome (read-line)]
      (if (empty? nome) 
        (do
          (println "Cadastro concluido.")
          (into alunos-atuais novos-alunos))
        (do
          (println (str "Digite a nota de " nome ":"))
          (let [nota (try
                       (Double/parseDouble (read-line))
                       (catch Exception _
                         (println "Nota invalida, definindo como 0.0")
                         0.0))]
            (recur (conj novos-alunos {:nome nome :nota nota})))))))) 

(defn relatorio-notas
  "Implementa a Opção 2: Relatorio de notas."
  [alunos]
  (println "\nRelatorio de Notas")
  (if (empty? alunos)
    (println "Nenhum aluno cadastrado.")
    (let [alunos-com-status (map adicionar-status alunos) 
          aprovados (filter #(= (:status %) "Aprovado") alunos-com-status) 
          media-geral (calcular-media alunos)] 
      (println "Alunos Aprovados:")
      (if (empty? aprovados)
        (println "Nenhum aluno aprovado.")
        (doseq [aluno aprovados]
          (println (str "\tNome: " (:nome aluno) ", Nota: " (:nota aluno)))))

      (println "--------------------------")
      (printf "Media Geral da Turma: %.2f\n" media-geral))))

(defn estatisticas-gerais
  "Implementa a Opção 3: Estatisticas gerais."
  [alunos]
  (println "\nEstatisticas Gerais")
  (if (empty? alunos)
    (println "Nenhum aluno cadastrado.")
    (let [total-alunos (count alunos) 
          alunos-com-status (map adicionar-status alunos)
          num-aprovados (count (filter #(= (:status %) "Aprovado") alunos-com-status))
          num-reprovados (- total-alunos num-aprovados) 
          media-geral (calcular-media alunos) 
          notas (map :nota alunos)
          maior-nota (apply max notas) 
          menor-nota (apply min notas)] 

      (println "Total de alunos cadastrados:" total-alunos)
      (println "Numero de aprovados:" num-aprovados)
      (println "Numero de reprovados:" num-reprovados)
      (printf "Maior nota: %.2f\n" maior-nota)
      (printf "Menor nota: %.2f\n" menor-nota)
      (printf "Media geral da turma: %.2f\n" media-geral))))

(defn exibir-menu
  "Exibe o menu principal formatado."
  []
  (println "\nMENU PRINCIPAL")
  (println "1 - Cadastrar Alunos")
  (println "2 - Relatorio de Notas")
  (println "3 - Estatisticas Gerais")
  (println "0 - Sair")
  (print "Escolha uma opcao: ")
  (flush)) 

(defn -main
  "Função principal que executa o loop do menu."
  [& args]
  (loop [alunos []] 
    (exibir-menu)
    (let [opcao (read-line)]
      (cond 
        (= opcao "1") (recur (cadastrar-alunos alunos)) 
        (= opcao "2") (do (relatorio-notas alunos) (recur alunos))
        (= opcao "3") (do (estatisticas-gerais alunos) (recur alunos)) 
        (= opcao "0") (println "Saindo...")
        :else (do
                (println "Opcao invalida. Tente novamente.")
                (recur alunos))))))