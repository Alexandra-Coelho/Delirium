![logo_escola_eng](https://user-images.githubusercontent.com/96354274/176214605-90adc5b2-1813-4de8-99ae-37421456769c.png)

# Delirium
GLM’s em Data Science como ferramenta na seleção de fatores de risco: um exemplo de previsão de *delirium*


## Contexto
A presente dissertação foi realizada no âmbito do Mestrado em Bioinformática, a qual visa a implementação de uma aplicação *web* que irá auxiliar os profissionais de saúde no diagnóstico dos diferentes subtipos de *delirium* no cenário de SU. 

Não obstante, também se aspira determinar, com base nos dados de entrada de um paciente no SU, quais os fatores de risco mais contributivos para a previsão do desenvolvimento de cada subtipo de *delirium*. Para tal, conforme os dados disponibilizados, serão explorados diversos algoritmos estatísticos que, posteriormente, serão aplicados nos GLM’s mais apropriados em consonância com o tema. Por fim, será selecionado o que exiba melhores resultados com base em métricas de precisão distintas e seleção dos fatores de risco mais relevantes para a previsão da desordem. O modelo selecionado foi implementado na aplicação.

## Abstract

Globalmente, 25\% da população sofre de distúrbios mentais, sendo possível implementar metodologias que possibilitem a deteção e previsão numa fase mais precoce.
Concretamente, o *delirium* é uma disfunção neuropsiquiátrica aguda, prevalente em doentes admitidos em contexto hospitalar de internamento e terapia intensiva. Sendo uma manifestação multifatorial é normalmente subdiagnosticada e negligenciada. 
O *delirium* pode ser categorizado, de acordo com o perfil de atividade motora, em hipoativo e hiperativo. 
Neste contexto, surge o tema da dissertação que visa desenvolver uma aplicação capaz de prever a ocorrência de *delirium* e dos seus subtipos, com base na metodologia dos GLMs.

Os modelos de regressão logística multinomial são frequentemente implementados para identificar as variáveis mais contributivas, dado que permitem modelar a relação entre os preditores e uma variável dependente multicategórica.
As etapas que precedem a implementação do algoritmo dizem respeito ao pré-processamento dos dados. 
No decorrer do processo de modelação, aplicou-se o ADASYN para gerar amostras sintéticas devido ao desbalanceamento das classes da variável dependente.
Posteriormente, foi realizada a seleção de variáveis recorrendo a diversas técnicas, sendo que o método *Elastic Net* com um *alpha* de 0,1 foi o que demonstrou um melhor desempenho. Para tal, este modelo foi implementado na aplicação disponível em https://alexandra-coelho.shinyapps.io/Delirium_detection/.

Para o subtipo hipoativo, permitiu a seleção de 27 variáveis, tendo obtido uma AUC-PR de 0,307 e uma AUC-ROC de 0,691. As variáveis mais contributivas incluem o período de internamento em dias, o alcoolismo, os analgésicos, os cardiotónicos, assim como, o grupo de diagnóstico referente à toxicidade e drogas.
Relativamente ao subtipo hiperativo, o modelo determinou 29 variáveis relevantes, onde obteve um valor de AUC-PR de 0,074 e de 0,531 para a AUC-ROC. Das variáveis mais impactantes destacam-se a PCR, a idade, a pO2, os critérios SIRS e o local de proveniência no SU, nomeadamente, o UDC1.
Especula-se que os baixos valores associados essencialmente ao subtipo hiperativo são devidos à baixa representatividade desta categoria.
Apesar deste modelo preditivo ainda poder ser melhorado, assume-se como uma ferramenta útil para os profissionais de saúde aquando o diagnóstico do *delirium* no SU.

Um dos principais resultados é a pipeline de implementação do melhor modelo a selecionar utilizando a regressão logística multinomial, tal como esquematizado com as principais etapas e resultados.



![git](https://github.com/Alexandra-Coelho/Delirium/assets/96354274/dbe98a24-92b4-4dae-85a0-1cbe05604974)
