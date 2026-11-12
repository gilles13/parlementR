# La DILA : rendre le droit et l'information publique accessibles

## Pourquoi une vignette sur la DILA ?

L’étude de l’activité parlementaire ne s’arrête pas aux débats, aux
amendements et aux votes. Pour comprendre le parcours d’un texte, il
faut aussi savoir **où trouver le droit publié**, les informations
administratives et les ressources qui éclairent les politiques
publiques.

La **Direction de l’information légale et administrative (DILA)** occupe
une place centrale dans cet écosystème. Elle est une direction
d’administration centrale des services du Premier ministre, placée sous
l’autorité de la secrétaire générale du Gouvernement.

## Une institution issue de deux histoires

Créée en **2010**, la DILA résulte de la fusion de la **direction des
Journaux officiels** et de la **direction de la Documentation
française**. Historiquement tournée vers l’impression et la diffusion de
publications officielles, elle produit aujourd’hui principalement des
services numériques.

Ses missions sont définies par le **décret n° 2010-31 du 11 janvier
2010**.

## Quatre missions complémentaires

### 1. L’information régalienne : publier et rendre le droit accessible

La DILA produit et publie le **Journal officiel**, assure la
consolidation de nombreux textes normatifs et diffuse les règles de
droit au moyen de **[Légifrance](https://www.legifrance.gouv.fr/)**.
Elle gère également les systèmes d’information nécessaires à la
production et à la diffusion de ces textes.

**Pour `parlementR` :** les travaux parlementaires permettent de suivre
l’élaboration d’un texte ; la publication officielle et la consultation
du droit en vigueur relèvent d’une étape distincte. Un amendement adopté
n’est pas, à lui seul, une disposition du droit applicable.

### 2. L’information économique : publier les annonces légales

La DILA diffuse notamment les annonces civiles et commerciales
(**BODACC**), les annonces de marchés publics (**BOAMP**), les annonces
légales obligatoires (**BALO**) et celles des associations et fondations
(**JOAFE**).

Cette mission contribue à la transparence de la vie économique,
financière et associative.

### 3. L’information administrative : expliquer les droits et les démarches

La DILA produit les contenus de référence de **[Service
Public](https://www.service-public.gouv.fr/)** et de son espace destiné
aux entreprises. Elle propose des informations pratiques, des parcours
de démarches, des simulateurs et des outils d’accompagnement des
usagers.

Cette information ne doit pas être confondue avec la publication
officielle des textes normatifs : elle aide à comprendre et à accomplir
des démarches.

### 4. L’information citoyenne : comprendre les institutions et les politiques publiques

La DILA anime **[Vie-publique.fr](https://www.vie-publique.fr/)**, qui
propose des contenus pédagogiques sur les institutions, les lois, les
politiques publiques et les sujets de société. Elle met aussi à
disposition des rapports publics, des discours publics et des dossiers
législatifs, et publie des ouvrages sous la marque **La Documentation
française**.

Cette mission rejoint directement l’objectif de `parlementR` : faciliter
l’accès aux sujets débattus et permettre à chacun de se documenter à
partir de références vérifiables.

## Où chercher quelle information ?

| Besoin | Ressource | Usage |
|:---|:---|:---|
| Retrouver un dossier, ses documents et ses amendements | Données parlementaires exploitées par `parlementR` | Suivre l’activité et les travaux parlementaires |
| Consulter un texte publié ou le droit en vigueur | [Légifrance](https://www.legifrance.gouv.fr/) | Vérifier le texte officiel et sa version consolidée |
| Comprendre une démarche ou une obligation administrative | [Service Public](https://www.service-public.gouv.fr/) | Accéder à une information pratique et actualisée |
| Comprendre un débat ou une politique publique | [Vie-publique.fr](https://www.vie-publique.fr/) | Lire des analyses pédagogiques et des dossiers documentaires |
| Rechercher une annonce légale | Sites d’annonces diffusés par la DILA | Consulter les publications économiques et associatives |

## La place de `parlementR`

`parlementR` ne se substitue ni aux sources parlementaires officielles,
ni à Légifrance, ni à Vie-publique.fr. Le package vise à **organiser et
explorer localement les données parlementaires**, à rechercher des
sujets et à produire des documents lisibles qui conservent les
références nécessaires à la vérification.

Une démarche documentaire cohérente consiste à :

1.  **Repérer un sujet** avec
    [`rechercher_dossiers()`](https://gilles13.github.io/parlementR/reference/rechercher_dossiers.md)
    ou
    [`produire_sujets()`](https://gilles13.github.io/parlementR/reference/produire_sujets.md).
2.  **Explorer son dossier** avec
    [`produire_dossier()`](https://gilles13.github.io/parlementR/reference/produire_dossier.md)
    et relever les références officielles.
3.  **Vérifier les textes publiés et leur état juridique** sur
    Légifrance, lorsque la question porte sur le droit en vigueur.
4.  **Compléter le contexte** avec les ressources de Vie-publique.fr ou
    de Service Public, selon la question posée.

Les recherches et traitements de `parlementR` restent locaux ; ces sites
constituent des **ressources documentaires externes**, et non des
services interrogés à chaque recherche.

## Source institutionnelle

- Direction de l’information légale et administrative, **[« Qui
  sommes-nous ?
  »](https://www.dila.gouv.fr/institution/presentation/qui-sommes-nous)**,
  page mise à jour le **21 septembre 2026**, consultée le 11 novembre
  2026.

Cette vignette présente les missions de la DILA telles qu’exposées par
l’institution. Elle ne prétend pas que la DILA produit elle-même
l’ensemble des données parlementaires exploitées par `parlementR`.
