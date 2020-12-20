--- 
author: 
  - "Leonardo Scoppitto"
  - "Matricola 545615"
classoption: a4paper
date: "Dicembre 2020"
documentclass: article
fontsize: 10pt
geometry: "left=3.5cm,right=3cm,top=3cm,bottom=3cm"
output: pdf_document
title: Secondo Progetto Intermedio
---

# Introduzione ed esecuzione

Il secondo progetto intermedio consiste nell'estendere un linguaggio funzionale didattico per introdurre il tipo di dato `Set`. Un `Set` è una collezione omogenea di dati, non ordinati e  che non contiene duplicati.\
Per eseguire il test è sufficiente spostarsi da terminale all'interno della cartella del progetto ed inserire il comando `./TestEseguibile`. Consiglio di espandere la finestra del terminale a tutto schermo così da leggere meglio l'output, essendo molto denso di scritte.\
Alternativamente è possibile copiare e incollare il contenuto del sorgente `TestInterpreteNoPrintf.ml` nella finestra interattiva del tool online [Try OCaml Pro](https://try.ocamlpro.com/).\
Per quanto riguarda la consultazione del codice sorgente, nella cartella `src` sono presenti i seguenti file:

* `TestInterprete.ml` $\rightarrow$ è il file che ha generato l'eseguibile `TestEseguibile`

* `TestInterpreteNoPrintf.ml` $\rightarrow$ esegue le stesse operazioni di `TestInterprete.ml`, ma sono state rimosse le chiamate alla funzione `Printf.printf`, così da avere la parte di codice dei test più leggibile, e sono stati aggiunti i commenti per charire tutte le scelte implementative.

* `InterpreteOriginale.ml` $\rightarrow$ è il file contenente l'interprete originale.

# Dettagli implementativi

Il tipo `Set`, dal punto di vista pratico, non è altro che una coppia `(lista, tipo)` in cui la lista può essere inizializzata come vuota o con un solo elemento (Singleton), mentre tipo è una stringa che identifica, appunto, il tipo di ogni elemento della lista, così da garantire l'omogeneità dell'insieme e la facilità nel typechecking, infatti è garantita la proprietà $\forall elemento \in lista, typecheck(elemento, tipo) = True$.

## Operazioni di base

### Union, Intersection e Difference
Le tre funzioni prendono come parametri due set, s1 e s2, verificano che i tipi siano compatibili e infine restituiscono, rispettivamente, un set contenente una lista con gli elementi di entrambi i set senza duplicati (proprietà garantita dalla funzione `list_as_set`), un set contenente una lista con gli elementi comuni di s1 e s2 e un set contenente gli elementi di s1, ma nessuno di s2.

## Operazioni sui Set

### Inserimento e rimozione

Entrambe le funzioni (`Insert` e `Rm`) prendono in input un set e un elemento. Dopo aver effettuato il typechecking dell'elemento, `Insert` effettua una ricerca dello stesso all'interno del set e, se non viene trovato nessun valore uguale, si effettua un inserimento in testa alla lista del set, mentre `Rm` passa la lista a una funzione di appoggio `delete`, che scorrerà tutta la lista rimuovendo eventualmente l'elemento richiesto.

### IsEmpty, IsIn, IsSubset

`IsEmpty` prende come parametro un set e tramite patter matching valuta se la lista del set è vuota, restituendo `Bool(true)` se la proprità è verificata, `Bool(false)` altrimenti.\
`IsIn` prende in inglesso due parametri, un set e un valore, ed esegue una ricerca del valore all'interno della lista del set, restituendo `Bool(true)` se la proprità è verificata, `Bool(false)` altrimenti.\
`IsSubset` prende come parametri di ingresso due set, s1 ed s2, e verifica se ogni elemento di s1 è contenuto in s2, della lista del set, restituendo `Bool(true)` se la proprità è verificata, `Bool(false)` altrimenti.\

### Getmin e Getmax

Entrambe prendono in input un solo set, la cui lista associata viene passata come parametro a due funzioni di supporto (`findmin` e `findmax`).

## Operazioni di carattere funzionale

