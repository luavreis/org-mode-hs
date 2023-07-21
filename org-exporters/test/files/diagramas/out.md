# O que são tensores? {#o-que-sao-tensores}

Em várias partes da ciência e da matemática, precisamos lidar com um conjunto de vetores e obter um outro vetor ou escalar como resultado; um dos exemplos mais simples é o de calcular o volume de paralelogramos, paralelepípedos e seus correspondentes em dimensões maiores. Por conta da estrutura linear dos espaços vetoriais, é natural considerar que a transformação envolvida seja multilinear, agindo como uma transformação linear quando são fixadas todas menos uma das entradas. Por exemplo, uma função $f:\R^2\times\R^2\to\R$ que calcula o volume do paralelogramo gerado por dois vetores em $\R^2$ é 2-linear, e na segunda entrada isso significa que $f(u,v+\lambda w)=f(u,v)+\lambda f(u,w)$.

A ideia dos tensores é construir uma correspondência entre as funções multilineares, que têm domínios em produtos cartesianos de espaços vetoriais, e funções lineares, com domínio no *produto tensorial* desses espaços. Com isso em mente, definimos este produto especificando as propriedades que ele deve ter: se $V_1,\dots,V_k$ são espaços vetoriais, o seu produto tensorial constitui uma dupla $(\otimes,V)$ de um espaço vetorial $V$ e uma função $k~$-linear $\otimes:\prod_{i=1}^k V_i\to V$ que satisfazem uma "propriedade universal": para toda função $k~$-linear $\psi:\prod_{i=1}^k V_i\to W$, existe uma única função linear $\tilde\psi:V\to W$ tal que $\psi=\tilde\psi\circ\otimes$. Podemos mostrar que a imagem de $\otimes$ gera $V$ (mesmo sendo, em geral, menor que $V$), os elementos de $V$ são chamados de tensores, e $V$ é denotado por $V_1\otimes...\otimes V_k$. A correspondência entre formas multilineares e mapas lineares com domínio no produto tensorial fica condensada na seguinte preposição:

::: proposition
Para todo espaço vetorial $W$, a função

$$
\begin{equation*}
  \Gamma:\Hom_{\F}^k(V_1,...,V_k, W)\to\Hom_{\F}(V_1\otimes...\otimes V_k,W)
\end{equation*}
$$

dada por $\Gamma(\psi)=\tilde\psi$ é um isomorfismo linear entre os dois espaços, com inversa $\Gamma^{-1}(f)=f\circ\otimes$.
:::

## Uma pequena motivação: teoria quântica {#uma-pequena-motivacao-teoria-quantica}

É bem sabido que o produto tensorial tem um papel bastante relevante na teoria quântica. Na teoria, os estados de um sistema são vetores em um espaço vetorial complexo com produto interno (um espaço de Hilbert). Quando queremos "juntar" vários sistemas quânticos para formar um novo sistema, o sistema resultante tem como espaço de estados o produto tensorial dos espaços originais. Nesse espaço produto, aparecem os tensores não puros, tensores que não estão na imagem da função $\otimes$: vistos assim, eles são justamente os estados que não podem ser formados pelo produto de um estado de cada componente, e são chamados de estados *emaranhados*.

Transformações em um sistema quântico são representadas por operadores lineares unitários (operadores sobrejetivos que preservam o produto interno). Se temos dois sistemas $\cat{H}_1,\cat{H}_2$ com operatores $f:\cat{H}_1\to\cat{H}_1$ e $g:\cat{H}_2\to\cat{H}_2$, podemos nos perguntar se faz sentido existir uma transformação em $\cat{H}_1\otimes\cat{H}_2$ que representa as duas transformações sendo feitas em paralelo, ao mesmo tempo:

::: proposition
Sejam $V_1,V_2,W_1,W_2$ espaços vetoriais. Então existe e é única uma transformação linear injetora

$$
\begin{equation*}
\Omega :\Hom_{\F}(V_1,W_1) \otimes \Hom_{\F}(V_2,W_2)\to\Hom_{\F}(V_1\otimes W_1,V_2\otimes W_2)
\end{equation*}
$$

tal que para todas as transformações lineares $S:V_1\to W_1$, $T:V_2\to W_2$ e quaisquer vetores $v_1\in V_1$ e $v_2\in V_2$, vale $\Gamma(S \otimes T)(v_1 \otimes v_2)=S(v_1)\otimes T(v_2)$. Para simplificar a notação, passaremos a chamar $\Gamma(S \otimes T)$ simplesmente de $S\otimes T$.
:::

É intuitivo pensar em "processos" que agem sobre os sistemas compondo operadores em paralelo ou sequencialmente. Como podemos usar desenhos de maneira formal para pensar sobre processos que envolvem tensores?

# Diagramas {#diagramas}

Os diagramas são compostos de caixas e fios, onde as caixas representam funções lineares e os fios servem como forma compor essas transformações. Para deixar a notação mais clara, podemos etiquetar as caixas com os nomes das transformações e os fios com os respectivos espaços vetoriais que são domínios ou contradomínios dessas transformações. Como exemplo, veja a transformação $f:V\to W$ abaixo. Quando as funções envolvidas agem sobre produtos tensoriais, como $g:V_1\otimes\dots\otimes V_n\to W_1\otimes\dots\otimes W_m$, desenhamos as caixas com vários fios que entram ou saem, um para cada fator do produto:

$$
\begin{tikzpicture}[disp]
 \matrix (c) {
    \coordinate (s); \\
    \node[below= .6 of s, box] (f) {$f$};
    \coordinate[below= .6 of f] (e); \\
  };
  \draw[oes={->-}]
  (s) -- node[vs] {$V$}
  (f) -- node[vs] {$W$} (e);

  \matrix[right= 3 of f] (c) {
      \node[box] (f) {$g$};
      \heads[2]{f}{0}
      \tails[2]{f}{2}\\
  };
  \node[vs,left= of f-hs-1] {$V_1$};
  \node[vs,right=of f-hs-2] {$V_n$};
  \node[vs,between= f-hs-1 and f-hs-2,yshift=1mm,xshift=.7mm]
  {$...$};
  \node[vs,left= of f-ts-2] {$W_1$};
  \node[vs,right=of f-ts-1] {$W_m$};
  \node[vs,between= f-ts-1 and f-ts-2,yshift=1mm,xshift=.7mm]
  {$...$};
\end{tikzpicture}
$$

Conhecemos duas "receitas de bolo" para produzir uma transformação linear a partir de outras duas, $f$ e $g$: o produto tensorial $f \otimes g$, e, se $f$ e $g$ concordarem domínio com contradomínio, a composta $f\circ g$. Nos diagramas, a composição de duas funções é ilustrada como na figura abaixo. Note que se tivermos mais caixas empilhadas, a associatividade da composição garante que não precisamos nos importar com parênteses.

$$
\begin{tikzpicture}[disp]
  \matrix (c2) {
    \node (s)   {};
    \node[below= .6 of s, box  ] (f)   {$f$};
    \node[below= .6 of f, box  ] (g)   {$g$};
    \node[below= .6 of g       ] (e)   {}; \\
  };
  \draw[oes={->-}] (s)
  -- node[vs] {$V$} (f)
  -- node[vs] {$W$} (g)
  -- node[vs] {$U$} (e);

  \defeq{c2};

  \matrix[rofeq   ] {
    \node (s)  {};
    \node[below= .6 of s, box  ] (gof) {$g\circ f$};
    \node[below= .6 of gof     ] (e)  {}; \\
  };
  \draw[oes={->-}] (s)
  -- node[vs] {$V$} (gof)
  -- node[vs] {$U$} (e);
\end{tikzpicture}
$$

Representamos produtos tensoriais de funções ao colocar as suas caixas lado a lado:

$$
\begin{tikzpicture}[disp]
  \matrix (c) {
    \node (s1) {};
    \node[below= .8 of s1, box ] (f)   {$f$};
    \node[below= .8 of f       ] (e1)  {};
    \node[right= .8 of s1      ] (s2)  {};
    \node[below= .8 of s2, box ] (g)   {$g$};
    \node[below= .8 of g       ] (e2)  {}; \\
  };
  \draw[oes={->-}] (s1)
  -- node[vs] {$V$} (f)
  -- node[vs] {$W$} (e1);

  \path[oes={->-}] (s2)
  -- node[vs] {$X$} (g)
  -- node[vs] {$U$} (e2);

  \eq{c};

  \matrix[rofeq] (c) {
    \node (s1) {};
    \node[below= .8 of s1, box ] (f)   {$f$};
    \node[below= .8 of f       ] (e1)  {};
    \node[right= .8 of s1      ] (s2)  {};
    \node[below= .8 of s2, box ] (g)   {$g$};
    \node[below= .8 of g       ] (e2)  {}; \\
  };
  \draw[oes={->-}] (s1)
  -- node[vs] {$V$} (f)
  -- node[vs] {$W$} (e1);

  \path[oes={->-}] (s2)
  -- node[vs] {$X$} (g)
  -- node[vs] {$U$} (e2);

  \node[box,fit=(f) (g)] {};

  \defeq{c};

  \matrix[rofeq] (c3) {
    \node (s1) {};
    \node[below= 2.1 of s1      ] (e1)  {};
    \node[right= 1.  of s1      ] (s2)  {};
    \node[below= 2.1 of s2      ] (e2)  {};
    \node[between=s1 and e2, box] (c) {$f\otimes g$}; \\
  };
  \draw[oes={->-}] (s1)
  to[out=-90,in=120] node[vs] {$V$} (c)
  to[out=-120,in=90] node[vs] {$W$} (e1);

  \path[oes={->-}] (s2)
  to[out=-90,in=60] node[vs] {$X$} (c)
  to[out=-60,in=90] node[vs] {$U$} (e2);
\end{tikzpicture}
$$

Em geral, os diagramas sempre corresponderão a transformações lineares que saem de um produto tensorial de espaços e vão a outro produto tensorial. De fato, os diagramas podem ser convertidos para a notação algébrica usual ao serem lidos linha por linha como uma composição de funções, e vice-versa. Mesmo sendo equivalentes à notação usual, a mágica dos diagramas é que a representação gráfica deixa algumas das regras para manipular tensores mais intuitivas.

Seguindo as definições que vimos acima, os diagramas de tensores têm uma estrutura formada por linhas, onde cada linha corresponde a um produto tensorial de funções e os fios indicam como compor as linhas verticalmente. Como as linhas dos diagramas são lidas de cima para baixo, em muitas passagens ficará mais claro para a leitura introduzir a notação $g\comp f\comp...\comp h$ para significar a composição $h\circ...\circ f\circ g$ de funções escrita na ordem reversa (assim, a leitura da esquerda para direita acompanha a leitura de cima para baixo do diagrama), e usaremos ela no decorrer do texto. A função identidade é ilustrada por um fio vazio e vertical `\tikz[baseline=-3.5mm,thick,minimum width=4mm]{\draw[->-] (0,0)--(0,-5mm);}`{=todo} . Note que pela igualdade $(g\comp f)\otimes(h\comp w)=(g\otimes h)\comp(f\otimes w)$, não precisamos nos preocupar com a ordem que interpretamos as operações composição e produto:

$$
\begin{tikzpicture}[disp]

  \matrix (c) {
    \node (s1) {};
    \node[below= .5 of s1, box  ] (g)   {$g$};
    \node[below= .5 of g,  box  ] (f)   {$f$};
    \node[below= .5 of f        ] (e1)  {};
    \node[right=  1 of s1       ] (s2)  {};
    \node[below= .5 of s2, box  ] (h)   {$h$};
    \node[below= .5 of h,  box  ] (w)   {$w$};
    \node[below= .5 of w        ] (e2)  {}; \\
  };
  \draw[->] (s1)--(g)--(f)--(e1);
  \draw[->] (s2)--(h)--(w)--(e2);
  \node[box,fit=(f) (g)] {};
  \node[box,fit=(h) (w)] {};

  \eq{c};

  \matrix[rofeq] (c) {
    \node (s1) {};
    \node[below= .5 of s1, box  ] (g)   {$g$};
    \node[below= .5 of g,  box  ] (f)   {$f$};
    \node[below= .5 of f        ] (e1)  {};
    \node[right=  1 of s1       ] (s2)  {};
    \node[below= .5 of s2, box  ] (h)   {$h$};
    \node[below= .5 of h,  box  ] (w)   {$w$};
    \node[below= .5 of w        ] (e2)  {}; \\
  };
  \draw[->] (s1)--(g)--(f)--(e1);
  \draw[->] (s2)--(h)--(w)--(e2);

  \eq{c};

  \matrix[rofeq] (c) {
    \node (s1) {};
    \node[below= .5 of s1, box  ] (g)   {$g$};
    \node[below= .5 of g,  box  ] (f)   {$f$};
    \node[below= .5 of f        ] (e1)  {};
    \node[right=  1 of s1       ] (s2)  {};
    \node[below= .5 of s2, box  ] (h)   {$h$};
    \node[below= .5 of h,  box  ] (w)   {$w$};
    \node[below= .5 of w        ] (e2)  {}; \\
  };
  \draw[->] (s1)--(g)--(f)--(e1);
  \draw[->] (s2)--(h)--(w)--(e2);
  \node[box,fit=(g) (h)] {};
  \node[box,fit=(f) (w)] {};
\end{tikzpicture}
$$

Mas basta considerar diagramas com mais de duas colunas que vemos que outros detalhes do formalismo também ficam escondidos na maneira que representamos os diagramas; por exemplo, quando colocamos três funções $f,g$ e $h$ lado a lado, subentende-se ser possível identificar as funções $f\ot (g\ot h)$ e $(f\ot g)\ot h$ entre si de "maneira natural", mesmo que elas atuem em espaços diferentes. Por enquanto, vamos subentender que sempre que preciso, aplicamos os isomorfismos canônicos entre esses espaços para que se adequem aos domínios e contradomínios das funções -- é possível abordar essas noções de maneira um pouco mais formal a partir de transformações naturais e outros conceitos da teoria das categorias, mas não faremos isso aqui.

Podemos representar um escalar $\lambda\in\F$ ou um vetor $v\in V$ como na figura abaixo:

::: latexfigure
$$
\begin{tikzpicture}[disp]
    \centering
    \matrix (c) {
      \node[vec] (v) {$v$};
      \coordinate[below= .6 of v] (e) {}; \\
    };
    \node[vec,above= 1 of c.south] (lam) {$\lambda$};
    \draw[oes={->-}] (v)
    -- node[vs] {$V$} (e);
  \end{tikzpicture}
$$
:::

Por que falamos de transformações antes de escalares ou vetores? A razão para isso é que do ponto de vista formal, as coisas ficam um pouco mais simples se pensarmos que escalares e vetores também são funções lineares: cada escalar $\lambda$ pode ser visto como a transformação $\lambda:\F\to\F$ dada por $t\mapsto\lambda t$, e cada vetor $v$ pode ser representado pela função $v:\F\to V$ definida por $t\mapsto tv$. Com essa mudança, todos os elementos dos diagramas serão transformações lineares, e os fios que carregam escalares ficam invisíveis. Não precisamos nos preocupar com eles pois sempre que preciso, lançamos mão do isomorfismo canônico $\F\otimes V\cong V$ dado por $\lambda\otimes v\mapsto\lambda v$.

No mundo dos produtos tensoriais, existe um único isomorfismo $B_{V,W}:V\ot W\to W\ot V$ que troca os vetores $v\ot w\mapsto w\ot v$. Nos diagramas, ele é desenhado como um par `\tikz[baseline=-3.5mm,thick,minimum width=4mm]{\draw[oes={->-l}] (0,0) to[in=90,out=-90] (4mm,-5mm) (4mm,0) to[in=90,out=-90] (0,-5mm); }`{=todo} de fios trocados. Assim, o diagrama

::: latexfigure
$$
\begin{tikzpicture}[disp]
    \matrix (c) {
      \draw[oes={->-l}]
      (0,0) node[vs] {$V$} to[in=90,out=-90] (8mm,-1)
      to[in=90,out=-90] (0,-2)
      (8mm,0) node[vs,right] {$W$} to[in=90,out=-90] (0,-1)
      to[in=90,out=-90] (8mm,-2);\\};
    \eq[0]{c};
    \matrix[rofeq] {
      \draw[oes={->-}]
      (0,0)--(0,-1)--(0,-2)
      (8mm,0)--(8mm,-1)--(8mm,-2);\\
    };
  \end{tikzpicture}
$$
:::

expressa a igualdade $B_{V,W}\comp{}B_{W,V}=\id_{V\ot{}W}$. Como os diagramas são em si funções lineares, também podemos fazer com eles as operações usuais de um espaço vetorial. Por exemplo, se $\car(\F)\neq2$, o diagrama abaixo é uma função linear:

```{=todo}
\(\displaystyle
 \tikz[disp,baseline=-0.65ex]{\matrix {
  \node[box] (s) {$s_2$};
  \heads[2]{s}{0}
  \tails[2]{s}{2} \\
};} :=\;
\frac12\left(
\tikz[disp]{
  \matrix (c) {
  \draw[oes={->-l}]
  (0,0) to[in=90,out=-90] (8mm,-1)
  (8mm,0) to[in=90,out=-90] (0,-1);\\};
  \node[right=of c] (p) {+};
  \matrix[right=of p] {
    \draw[oes={->-}]
    (0,0)--(0,-1)
    (8mm,0)--(8mm,-1);\\
  };}\right)
\)
```
e o leitor curioso pode checar que $s_2\comp s_2=s_2$ usando a identidade que vimos logo acima. Outra transformação interessante é o funcional avaliação $\ev{V}: V\ot{}V^*\to\F$ definido por $\ev{V}(v\ot{f})=f(v)$. Nos inspirando na existência desse funcional, introduziremos a partir daqui uma notação para fios que carregam o espaço dual $V^*$ de um espaço $V$ ao representá-los como um fio também etiquetado por $V$, mas com a orientação (a seta) invertida. Com essa notação e com a justificativa de que $\ev{V}$ é o funcional "canônico" nesse espaço, passaremos a escrever o mapa $\ev{V}$ como um simples fio dobrado:

::: latexfigure
$$
\begin{tikzpicture}[disp]
    \matrix (c0) {
      \node[vec] (f) {$f$};
      \coordinate[below= .6 of f] (e);
      \draw[->-] (e)-- node[vs] {$V$} (f);\\
    };
    \defeq{c0};
    \matrix[rofeq] (c0) {
      \node[vec] (v) {$f$};
      \coordinate[below= .6 of v] (e);
      \draw[->-] (v)-- node[vs] {$V^*$} (e);\\
    };
    \matrix[right=2 of c0] (c1) {
      \node[vs] at (-1,0) {$V$};
      \tev[-1,0]{0,0}\\
    };
    \defeq{c1};
    \matrix[rofeq] (c) {
      \node[box] (ev) {$\displaystyle\ev{\scriptscriptstyle V}$};
      \fheads[2]{ev}{1}\\
    };
  \end{tikzpicture}
$$
:::

Assim, também fica simples descobrir algumas correspondências entre tensores e mapas lineares, por exemplo, $T:V^*\ot W\to\Hom_{\F}(V,W)$ dada por:

```{=todo}
\(
  \tikz[disp] {
    \matrix (c0) {
      \node[vec] (f) {$u$};
      \tails[2]{f}{1}
      \node[vs] at (f-t-2) {$V$};
      \node[vs,right] at (f-t-1) {$W$};\\
    };
  }
  \xmapsto{T}
  \tikz[disp] {
    \matrix (c0) {
      \node[vec] (w) {$u$};
      \coordinate[above left=1 of w] (t);
      \coordinate[below=1 of t] (b);
      \coordinate[below=.5 of b] (b1);
      \coordinate (w1) at ($(w)-(.5,.8)$);
      \draw[oes={->-}] (t) node[vs] {$V$} -- (b)
      .. controls (b1) and (w1).. (w);
      \draw[->-] (w) to[out=-60,in=90] ($(w)+(.5,-.8)$)
      node[vs,right] {$W$};\\
    };
  }
\)
```
Se interpretamos o diagrama na linguagem usual, obtemos:

$$
\begin{equation*}
 T(u)=(\id_V\ot\, u)
\comp (\ev{V}\ot\id_W).
\end{equation*}
$$

Mais explicitamente, $T(f\ot w)(v)$ pode ser calculado passo a passo como:

$$
\begin{equation*}
 v\xmapsto{\id_V\ot\, f\,\ot\, w} v\ot f\ot w
\xmapsto{\ev{V}\ot\id_W} f(v)\ot w
\xmapsto{\sim} f(v)w,
\end{equation*}
$$

ou seja, $T(f\ot w)(v)=f(v)w$.

## Dimensão finita {#dimensao-finita}

Diagramas de tensores envolvendo espaços de dimensões finitas são especiais. Nesse contexto, conseguimos definir um dual $\coev{V}$ do funcional $\ev{V}$ que tem o efeito de dobrar os fios para baixo, chamado de coavaliação. Explicitamente, em dimensão finita o morfismo $T$ visto acima é um isomorfismo, e definiremos $\coev{V}=T^{-1}(\id_V)$.

Note que essa definição não depende de uma escolha da base, mas se tomarmos uma base $\{e_i\}_{i=1}^n$ de $V$, podemos mostrar que $\coev{V}=\sum_{i=1}^n e_i^*\ot e_i$:

$$
\begin{equation*}
 T(\coev{V})(v)=T\left(\sum_{i=1}^n e_i^*\ot e_i\right)(v)
=\sum_{i=1}^nT(e_i^*\ot e_i)(v)
=\sum_{i=1}^n e_i^*(v)e_i=v.
\end{equation*}
$$

Com isso em mente, passaremos a representar $\coev{V}$ nos diagramas da seguinte forma:

::: latexfigure
$$
\begin{tikzpicture}[disp]
    \matrix (c) {
      \tcoev{1,0}\\
    };
    \eq{c};
    \matrix[rofeq] (c) {
      \node[box] (ev) {$\coev{V}$};
      \tails[2]{ev}{1}\\
    };
    \eq{c};
    \node[rofeq] {$\displaystyle1\mapsto\sum_{i=1}^{n}e^*_i\otimes e_i$};
  \end{tikzpicture}
$$
:::

As identidades mais importantes, que tanto justificam chamar $\coev{V}$ de dual de $\ev{V}$ como mostram uma "interpretação topológica" dos diagramas, são as *identidades zig-zag*:

$$
\begin{tikzpicture}[disp]
  \matrix[matd] (m) {
    &&\\&&\\&&\\
  };
  \tcoev[m-2-2]{m-2-3}
  \draw[->-] (m-1-1)--(m-2-1);
  \tev[m-2-1]{m-2-2}
  \draw[->-] (m-2-3)--(m-3-3);

  \eq{m};

  \matrix[rofeq] (m) {
    \draw[->-] (0,1.4) to (0,0);\\
  };

  \matrix[right= 2 of m,matd] (m) {
    &&\\&&\\&&\\
  };
  \tcoev[m-2-1]{m-2-2}
  \draw[->-] (m-2-3)--(m-1-3);
  \tev[m-2-2]{m-2-3}
  \draw[->-] (m-3-1)--(m-2-1);

  \eq{m};

  \matrix[rofeq] (m) {
    \draw[->-] (0,-1.4) to (0,0);\\
  };
\end{tikzpicture}
$$

Podemos provar essas identidades: a identidade da esquerda é, por definição, o fato que $T(\coev{V})=\id_V$, e na direita podemos mostrar a igualdade aplicando uma $f\in V^*$ ao diagrama, e assim obtendo:

$$
\begin{equation*}
 f\xmapsto{\coev{V}\ot\,\id_{V^*}}
\sum_{i=1}^n e_i^*\ot e_i\ot f
\xmapsto{\id_{V^*}\ot\,\ev{V}} \sum_{i=1}^n f(e_i)e_i^* = f.
\end{equation*}
$$

Usando o morfismo $B_{V^*,V}$, também podemos definir $\ev{V^*}$ e $\coev{V^*}$, respectivamente:

$$
\begin{tikzpicture}[disp]
  \matrix (c) {
    \tev[1,0]{0,0}
    \node[vs,above] at (1,0) {$V$};
    \node[vs,above] at (0,0) {$V^*$};\\
  };
  \defeq{c};
  \matrix[rofeq] (c) {
    \tev[0,0]{1,0}
    \draw
    (1,.8) to[in=90,out=-90] (0,0)
    (1,0) to[in=-90,out=90] (0,.8);\\
  };

  \matrix[right= 2 of c] (c) {
    \tcoev[1,0]{0,0}
    \node[vs,below] at (0,0) {$V$};
    \node[vs,below] at (1,0) {$V^*$};\\
  };
  \defeq{c};
  \matrix[rofeq] (c) {
    \tcoev{1,0}
    \draw
    (1,-.8) to[in=-90,out=90] (0,0)
    (1,0) to[in=90,out=-90] (0,-.8);\\
  };
\end{tikzpicture}
$$

e similarmente, podemos obter outras duas identidades zig-zag com essas versões. Observe que a notação também sugere uma identificação do bidual $(V^*)^*$ com o espaço $V$. Isso se justifica pela existência de um isomorfismo $\Phi:V\to (V^*)^*$ dado por $\Phi(v)(g)=g(v)$, que é canônico no sentido que não depende de uma escolha de base do espaço. O mesmo não vale para $V^*$, entretanto. Na linguagem categórica, os espaços vetoriais de dimensão finita formam uma *categoria fechada compacta*.

Poderíamos provar algumas igualdades apenas substituindo pedaços do diagrama por identidades anteriores, por exemplo:

::: latexfigure
$$
\begin{tikzpicture}[disp]
    \matrix[matd,column sep=7mm] (m) {
      &&\\&&\\&&\\&&\\
    };
    \draw[oes={-<-l}]
    (m-4-3)--(m-3-3) to[out=90,in=-90] (m-2-2);
    \tcoev[m-2-1]{m-2-2}
    \draw (m-2-1) -- (m-3-1);
    \tev[m-3-2]{m-3-1}
    \draw[oes={-<-l}]
    (m-3-2) to[out=90,in=-90] (m-2-3) -- (m-1-3);
    \node[box,fit=(m-1-1) (m-2-3),red] {};
    \eq{m};

    \matrix[rofeq,matd,column sep=7mm] (m) {
      &&\\&&\\&&\\&&\\&&\\&&\\
    };
    \draw[oes={-<-l}]
    (m-6-3)--(m-5-3) to[out=90,in=-90] (m-4-2)
    to[out=90,in=-90] (m-3-3)--(m-2-3);
    \tcoev[m-2-2]{m-2-3}
    \draw[oes={-<-l}]
    (m-2-2) to[out=-90,in=90] (m-3-1)--(m-4-1)--(m-5-1);
    \tev[m-5-2]{m-5-1}
    \draw[oes={-<-l}]
    (m-5-2) to[out=90,in=-90] (m-4-3)
    to[out=90,in=-90] (m-3-2)
    to[out=90,in=-90] (m-2-1) -- (m-1-1);
    \node[box,fit=(m-1-1) (m-4-3),red,opacity=.2] {};
    \node[box,fit=(m-5-2) (m-3-3),violet] {};

    \eq{m};
    \matrix[rofeq,matd,column sep=7mm] (m) {
      &&\\&&\\&&\\&&\\
    };
    \draw[oes={-<-l}]
    (m-4-3)--(m-2-3);
    \tcoev[m-2-2]{m-2-3}
    \draw[oes={-<-l}]
    (m-2-2) to[out=-90,in=90] (m-3-1);
    \tev[m-3-2]{m-3-1}
    \draw[oes={-<-l}]
    (m-3-2) to[out=90,in=-90] (m-2-1)--(m-1-1);
    \node[box,fit=(m-4-1) (m-2-2),olive] {};

    \eq{m};

    \matrix[rofeq,matd,column sep=7mm] (m) {
      &&\\&&\\&&\\
    };
    \tcoev[m-2-2]{m-2-3}
    \draw[->-] (m-1-1)--(m-2-1);
    \tev[m-2-1]{m-2-2}
    \draw[->-] (m-2-3)--(m-3-3);
    \node[box,fit=(m-2-1) (m-3-2),olive,opacity=.2] {};

    \eq{m};

    \matrix[rofeq] (m) {
      \draw[->-] (0,1.4) to (0,0);\\
    };
  \end{tikzpicture}
$$
:::

Mas de forma mais geral, é possível demonstrar que é sempre permitido
"desembaraçar" os diagramas. Faremos isso com algums lemas que mostram como uma
função $f$ comuta com os fios dobrados. Por exemplo, é simples ver que:

::: latexfigure
$$
\begin{tikzpicture}[disp]
    \matrix[matd,row sep=1.2cm] (m) {
      &\\&\\
    };
    \matrix (t) {
      \node[box] (f) at (m-2-1) {$f$};
      \coordinate[below=.2 of f] (fe);
      \draw (f)--(fe);
      \draw[->-l] (m-1-2) to[out=-90,in=90] (f);
      \draw[oes={->-l}] (m-1-1) to[out=-90,in=90]
      (m-2-2 |- f.north) -- (m-2-2 |- fe);\\
    };

    \eq{t};

    \matrix[rofeq,matd,row sep=1.2cm,yshift=-2.5mm] (m) {
      &\\&\\
    };
    \node[box] (f) at (m-1-2) {$f$};
    \coordinate[above=.2 of f] (fs);
    \draw (fs)--(f);
    \draw[->-l] (f) to[out=-90,in=90] (m-2-1);
    \draw[oes={->-l}] (m-1-1 |- fs) -- (m-2-1 |- f.south)
    to[out=-90,in=90] (m-2-2);
  \end{tikzpicture}
$$
:::

Definimos a *transposta* de funções lineares $f:V\to W$, $g:W^*\to V^*$ pelos diagramas:

::: latexfigure
$$
\begin{tikzpicture}[disp]
    \matrix (c) {
      \node[box] (ft) {$f^t$};
      \fheads{ft}{1}
      \ftails{ft}{0}\\
    };
    \defeq{c};
    \matrix[rofeq,matd] (m) {
      &&\\&&\\&&\\
    };
    \node[box] (f) at (m-2-2) {$f$};
    \tcoev[m-2-1 |- f.north]{f.north}
    \tev[f.south]{m-2-3 |- f.south}
    \draw[oes={->-}]
    (m-3-1) -- (m-2-1 |- f.north)
    (m-2-3 |- f.south) -- (m-1-3);

    \matrix[right=2 of m] (c) {
      \node[box] (ft) {$g^t$};
      \fheads{ft}{0}
      \ftails{ft}{1}\\
    };
    \defeq{c};
    \matrix[rofeq,matd] (m) {
      &&\\&&\\&&\\
    };
    \node[box] (f) at (m-2-2) {$g$};
    \tcoev[f.north]{m-2-1 |- f.north}
    \tev[m-2-3 |- f.south]{f.south}
    \draw[oes={->-}]
    (m-2-1 |- f.north) --(m-3-1)
    (m-1-3)--(m-2-3 |- f.south);
  \end{tikzpicture}
$$
:::

Podemos checar que a primeira transposta $f^t$ concorda com a transposta usual
ao aplicá-la a um vetor $w\in W^*$. Pelas definições do diagrama acima e de
$\ev{V}$, sabemos que para todo $v\in V$, $(f^t(w))(v)$ é igual a:

::: latexfigure
$$
\begin{tikzpicture}[disp]
    \matrix (c) {
      \node[vec] (w) {$w$};
      \node[box,below=.2 of w] (ft) {$f^t$};
      \node[vec,left=.3 of ft] (v) {$v$};
      \tev[v.south]{ft.south}
      \draw (ft)--(w);\\
    };
    \eq{c};
    \matrix[rofeq,matd] (m) {
      &&&\\&&&\\&&&\\
    };
    \node[box] (f) at (m-2-3) {$f$};
    \node[vec] (w) at (m-1-4) {$w$};
    \node[vec] (v) at (m-2-1) {$v$};
    \tev[v.south]{m-2-2 |- f.south}
    \tcoev[m-2-2 |- f.north]{f.north}
    \tev[f.south]{m-2-4 |- f.south}
    \draw[oes={->-}]
    (m-2-2  |- f.south) -- (m-2-2 |- f.north)
    (m-2-4 |- f.south) -- (w);
    \eq{m};
    \matrix[rofeq,matd] (m) {
      &\\&\\&\\
    };
    \node[box] (f) at (m-2-1) {$f$};
    \node[vec] (w) at (m-2-2) {$w$};
    \node[vec] (v) at (m-1-1) {$v$};
    \tev[f.south]{w.south}
    \draw (v)--(f);
    \eq{m};
    \node[rofeq,xshift=-3mm] {$(w\circ f)(v)$};
  \end{tikzpicture}
$$
:::

Como isso vale para todos $v$ e $w$, $f^t$ concorda com a transposta usual. Para $g^t$, isso é apenas parcialmente verdade; ela é a transposta usual "a menos" do isomorfismo canônico $\Phi$. Com a transposta, andamos pelos fios com os *lemas do deslizamento*:

::: latexfigure
$$
\begin{tikzpicture}[disp]
    \matrix[matd] (m) {
      &\\&\\&\\
    };
    \node[box] (f) at (m-2-1) {$f$};
    \tev[f.south]{m-2-2 |- f.south}
    \draw[oes={->-}]
    (m-1-1) -- (f.north)
    (m-2-2 |- f.south) -- (m-1-2);
    \eq{m};
    \matrix[rofeq,matd] (m) {
      &&&\\&&&\\&&&\\
    };
    \node[box] (f) at (m-2-3) {$f$};
    \tev[f.south]{m-2-4 |- f.south}
    \tcoev[m-1-2 |- f.north]{f.north}
    \tev[m-2-1 |- f.south]{m-2-2 |- f.south}
    \draw[oes={->-}]
    (m-1-1) -- (m-2-1 |- f.south)
    (m-2-2 |- f.south) -- (m-2-2 |- f.north)
    (m-2-4 |- f.south) -- (m-1-4);
    \eq{m};
    \matrix[rofeq,matd] (m) {
      &\\&\\&\\
    };
    \node[box] (f) at (m-2-2) {$f^t$};
    \tev[m-2-1 |- f.south]{f.south}
    \draw[oes={->-}]
    (f.north) -- (m-1-2)
    (m-1-1) -- (m-2-1 |- f.south);
  \end{tikzpicture}
$$
:::

Usando o mesmo argumento,

::: latexfigure
$$
\begin{tikzpicture}[disp]
    \matrix[matd] (m) {
      &\\&\\&\\
    };
    \node[box] (f) at (m-2-1) {$f^t$};
    \tcoev[f.north]{m-2-2 |- f.north}
    \draw[oes={->-}]
    (m-3-1) -- (f)
    (m-2-2 |- f.north) -- (m-3-2);
    \eq{m};
    \matrix[rofeq,matd] (m) {
      &&&\\&&&\\&&&\\
    };
    \node[box] (f) at (m-2-2) {$f$};
    \tev[f.south]{m-2-3 |- f.south}
    \tcoev[m-1-1 |- f.north]{f.north}
    \tcoev[m-2-3 |- f.north]{m-2-4 |- f.north}
    \draw[oes={->-}]
    (m-3-1) -- (m-2-1 |- f.north)
    (m-2-3 |- f.south) -- (m-2-3 |- f.north)
    (m-2-4 |- f.north) -- (m-3-4);
    \eq{m};
    \matrix[rofeq,matd] (m) {
      &\\&\\&\\
    };
    \node[box] (f) at (m-2-2) {$f$};
    \tcoev[m-2-1 |- f.north]{f.north}
    \draw[oes={->-}]
    (f) -- (m-3-2)
    (m-3-1) -- (m-2-1 |- f.north);
  \end{tikzpicture}
$$
:::

e também:

::: latexfigure
$$
\begin{tikzpicture}[disp]
  \matrix[matd] (m) {
    &\\&\\&\\
  };
  \node[box] (f) at (m-2-1) {$f$};
  \tcoev[m-2-2 |- f.north]{f.north}
  \draw[oes={->-}]
  (f) -- (m-3-1)
  (m-3-2) -- (m-2-2 |- f.north);
  \eq[0]{m};
  \matrix[right=of eq,matd] (m) {
    &\\&\\&\\
  };
  \node[box,yshift=10pt] (f) at (m-2-2) {$f$};
  \tcoev[m-2-1 |- f.north]{f.north}
  \draw[oes={->-}]
  (f) to[out=-90,in=90] (m-3-1)
  (m-3-2) to[out=90,in=-90] (m-2-1 |- f)
  (m-2-1 |- f) -- (m-2-1 |- f.north);
  \eq{m};
  \matrix[rofeq,matd] (m) {
    &\\&\\&\\
  };
  \node[box,yshift=10pt] (f) at (m-2-1) {$f^t$};
  \tcoev[f.north]{m-2-2 |- f.north}
  \draw[oes={->-l}]
  (m-3-2) to[out=90,in=-90] (f)
  (m-2-2 |- f) to[out=-90,in=90] (m-3-1)
  (m-2-2 |- f.north) -- (m-2-2 |- f);
  \eq[0]{m};
  \matrix[right=of eq,matd] (m) {
    &\\&\\&\\
  };
  \node[box] (f) at (m-2-2) {$f^t$};
  \tcoev[f.north]{m-2-1 |- f.north}
  \draw[oes={->-}]
  (m-3-2) -- (f)
  (m-2-1 |- f.north) -- (m-3-1);
\end{tikzpicture}
$$

$$
\begin{tikzpicture}[disp]
  \matrix[matd] (m) {
    &\\&\\&\\
  };
  \node[box] (f) at(m-2-1) {$f^t$};
  \tev[m-2-2 |- f.south]{f.south}
  \draw[oes={->-}]
  (f) -- (m-1-1)
  (m-1-2) -- (m-2-2 |- f.south);
  \eq{m};

  \matrix[rofeq,matd] (m) {
    &\\&\\&\\
  };
  \node[box] (f) at(m-2-2) {$f$};
  \tev[f.south]{m-2-1 |- f.south}
  \draw[oes={->-}]
  (m-1-2) -- (f)
  (m-2-1 |- f.south) -- (m-1-1);
\end{tikzpicture}
$$
:::

Os mesmos lemas valem para uma função $g:W^*\to V^*$ pelas mesmas provas, basta
inverter as setas. Mas também note que poderíamos ter definido as transpostas de
uma forma diferente, dobrando os fios na direção oposta. Porém, usando os lemas
anteriores, provar que essa transposta alternativa é igual a que já foi definida
é uma tarefa simples de "deslizar" a função:

::: latexfigure
$$
\begin{tikzpicture}[disp]
    \matrix[matd] (m) {
      &&\\&&\\&&\\
    };
    \node[box] (f) at (m-2-2) {$f$};
    \tcoev[m-2-3 |- f.north]{f.north}
    \tev[f.south]{m-2-1 |- f.south}
    \draw[oes={->-}]
    (m-2-1 |- f.south) -- (m-1-1)
    (m-3-3) -- (m-1-3 |- f.north);

    \eq{m};

    \matrix[rofeq,matd] (m) {
      &&\\&&\\&&\\
    };
    \node[box] (f) at (m-2-3) {$f^t$};
    \tcoev[f.north]{m-2-2 |- f.north}
    \tev[m-2-2 |-f.south]{m-2-1 |- f.south}
    \draw[oes={->-}]
    (m-3-3) -- (f)
    (m-2-2 |- f.north) -- (m-2-2 |- f.south)
    (m-2-1 |- f.south) -- (m-1-1);

    \eq{m};

    \matrix[rofeq] (c) {
      \node[box] (ft) {$f^t$};
      \fheads{ft}{1}
      \ftails{ft}{0}\\
    };
  \end{tikzpicture}
$$
:::

Se compormos as duas versões da transposta e aplicarmos a identidade zig-zag,
mostramos também que $(f^t)^t=f$ (para a transposta que definimos, claro).
Juntos, esses fatos garantem que podemos "desembaraçar" fios soltos nos
diagramas simplesmente deslizando as funções ao longo dos fios; no máximo, os
fios trocam a função para sua transposta ou vice-versa.

## Calculando com os diagramas {#calculando-com-os-diagramas}

Até então, nós aprendemos um formalismo relativamente abstrato para os diagramas, mas ainda não é fácil calcular valores numéricos. Para nos ajudar nisso, vamos escolher uma base $(e_i)_{i=1}^n$ do espaço $V$, e denotar $e^i=e_i^*$ para todo $i\in\{0,...,n\}$. O truque principal é uma decomposição da função identidade:

::: latexfigure
$$
\begin{tikzpicture}[disp]
    \matrix (m) {
      \draw[->-] (0,1.4) to (0,0);\\
    };
    \eq{m};
    \node[right=.1 of eq] (sum) {$\displaystyle\sum_{i=1}^d$};
    \matrix[right=.2 of sum,matd] (m) {
      \\ \\
    };
    \node[vec] (v) at (m-1-1) {$e^i$};
    \node[vec] (dv) at (m-2-1) {$e_i$};
    \ftails[1]{dv}{1}
    \fheads[1]{v}{0}
  \end{tikzpicture}
$$
:::

Estaremos sempre identificando $\F^*\cong\F$, de forma que $(e_i)^t=\Phi(e_i): V^*\to\F$ e $(e^i)^t=e^i : \F\to V^*$ (lembrando: os vetores estão sendo vistos como funções). As transpostas desses elementos serão representadas com a mesma etiqueta, mas com um fio na direção oposta (já que são essencialmente os mesmos). Por exemplo, um diagrama curioso:

::: latexfigure
$$
\begin{tikzpicture}[disp]
    \matrix (c) {
      \node[tbox] (f) {$f$};
      \coordinate[left= .5 of f] (a);
      \node[left= 0 of a,vs] {$V$}; \\
    };
    \draw[oes={->-}] (f)
    .. controls ($(f) - (0,1)$) and ($(a) - (0,1)$) .. (a)
    .. controls ($(a) + (0,1)$) and ($(f) + (0,1)$) .. (f);

    \eq{c};

    \node[rofeq] (sum) {$\displaystyle\sum_{i=1}^d$};
    \matrix[right=.2 of sum,matd] (m) {
      &\\&\\
    };
    \node[vec] (v) at (m-1-1) {$e_i$};
    \node[vec] (dv) at (m-2-1) {$e^i$};
    \node[box,between=m-2-2 and m-1-2] (f) {$f$};
    \tcoev[v.north]{f |- v}
    \tev[f |- dv]{dv.south}
    \draw
    (f |- v) -- (f) -- (f |- dv);
    \eq[.3]{m};

    \node[right=of eq] (sum) {$\displaystyle\sum_{i=1}^d$};
    \matrix[right=.2 of sum,matd] (m) {
      \\\\\\
    };
    \node[vec] (v) at (m-1-1) {$e_i$};
    \node[vec] (dv) at (m-3-1) {$e^i$};
    \node[box] (f) at (m-2-1) {$f$};
    \draw (v)--(f)--(dv);
    \eq[.3]{m};
    \node[right=of eq] {$\tr(f)$};
  \end{tikzpicture}
$$
:::

Com isso, fica bastante simples mostrar que $\tr(f\circ g)=\tr(g\circ f)$:

::: latexfigure
$$
\begin{tikzpicture}[disp]
    \matrix[matd] (m) {
      &\\&\\
    };
    \node[box] (f) at (m-1-2) {$f$};
    \node[box]  (g) at (m-2-2) {$g$};
    \tcoev[m-1-1 |- f.north]{f.north}
    \tev[g.south]{m-2-1 |- g.south}
    \draw
    (m-2-1 |- g.south) -- (m-1-1 |- f.north)
    (f) -- (g);

    \eq{m};

    \matrix[rofeq,matd] (m) {
      &\\
    };
    \node[box] (f) at (m-1-2) {$f$};
    \node[box] (g) at (m-1-1) {$g^t$};
    \tcoev[g.north]{f.north}
    \tev[f.south]{g.south}

    \eq{m};

    \matrix[rofeq,matd] (m) {
      &\\&\\
    };
    \node[box] (f) at (m-1-2) {$g$};
    \node[box] (g) at (m-2-2) {$f$};
    \tcoev[m-1-1 |- f.north]{f.north}
    \tev[g.south]{m-2-1 |- g.south}
    \draw
    (m-2-1 |- g.south) -- (m-1-1 |- f.north)
    (f) -- (g);
  \end{tikzpicture}
$$
:::
