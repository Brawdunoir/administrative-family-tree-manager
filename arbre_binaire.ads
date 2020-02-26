generic

    type T_Cle is private; -- Type clé générique.

package Arbre_Binaire is

	Cle_Presente_Exception_Bin : Exception;	-- une clé est déjà présente dans un arbre.
	Cle_Absente_Exception_Bin  : Exception;	-- une clé est absente d'un arbre.
	Emplacement_Reserve_Exception_Bin : Exception; -- L'emplacement du noeud que l'on veut ajouter est déjà pris.


	type T_Pointeur_Binaire is private; -- Type pointeur

   -- Initialiser un Arbre.
	procedure Initialiser(Arbre: out T_Pointeur_Binaire; Cle : in T_Cle);


	-- Savoir si un arbre est vide.
	-- Paramètre :  Arbre L'arbre que l'on veut tester.
	function Est_Vide (Arbre : in T_Pointeur_Binaire) return Boolean;

	-- Savoir si le sous arbre droit d'un arbre est vide.
	-- Paramètre :  Arbre L'arbre racine du sous arbre.
	function Est_Vide_Fils_Droit (Arbre : in T_Pointeur_Binaire) return Boolean;

	-- Savoir si le sous arbre gauche d'un arbre est vide.
	-- Paramètre :  Arbre L'arbre racine du sous arbre.
	function Est_Vide_Fils_Gauche (Arbre : in T_Pointeur_Binaire) return Boolean;

	-- Obtenir le sous arbre gauche d'un arbre.
	-- Paramètre :  Arbre L'arbre racine du sous arbre.
	function Retourner_Fils_Gauche (Arbre : in T_Pointeur_Binaire) return T_Pointeur_Binaire;


	-- Obtenir le sous arbre droit d'un arbre.
	-- Paramètre :  Arbre L'arbre racine du sous arbre.
	function Retourner_Fils_Droit (Arbre : in T_Pointeur_Binaire) return T_Pointeur_Binaire;



	-- Obtenir la clé d'un noeud.
	-- Paramètre :  Arbre Le pointeur qui pointe vers le noeud dont on veut connaire sa clé.
	function Retourner_Cle (Arbre : in T_Pointeur_Binaire) return T_Cle;


	-- Obtenir le nombre de fils à partir d'un noeud donné (lui compris).
    -- Paramètres : 
	-- 			Arbre L'arbre dans lequel on va effectuer le comptage.
	-- 			Cle La clé de l'individu à partir duquel on compte.
    -- Exception: Cle_Absente_Exception si Clé n'est pas utilisée l'arbre.
	function Nombre_Fils (Arbre : in T_Pointeur_Binaire; Cle : in T_Cle) return Integer with
		Post => Nombre_Fils'Result >= 1;


	
	function Nombre_Fils_Recu (Arbre : in T_Pointeur_Binaire) return Integer;


	-- Insérer un fils droit à un noeud dans l'arbre.
	-- Paramètres :
	--		Arbre l'arbre dans lequel on ajoute un noeud.
	--		Cle_Parent La cle du noeud auquel on ajoute un fils droit.
	-- 		Cle_Fils La cle du fils que l'on insère dans l'arbre.
	-- Exception : Cle_Presente_Exception si la clé est déjà dans l'arbre.
	procedure Ajouter_Fils_Droit (Arbre : in out T_Pointeur_Binaire; Cle_Parent : in T_Cle; Cle_Fils : in T_Cle);-- with
		--Post => Nombre_Fils (Arbre, Cle_Parent) = Nombre_Fils (Arbre, Cle_Parent)'Old + 1; -- un élément de plus

	-- Insérer un fils gauche à un parent dans l'arbre.
	-- Paramètres :
	-- 		Arbre l'arbre dans lequel on ajoute un noeud.
	-- 		Cle_Parent La cle du noeud auquel on ajoute un fils gauche.
	--		Cle_Fils La cle du fils que l'on insère dans l'arbre.
	-- Exception : Cle_Presente_Exception si la clé est déjà dans l'arbre.
	procedure Ajouter_Fils_Gauche (Arbre : in out T_Pointeur_Binaire; Cle_Parent : in T_Cle; Cle_Fils : in T_Cle);-- with
		--Post => Nombre_Fils (Arbre, Cle_Parent) = Nombre_Fils (Arbre, Cle_Parent)'Old + 1; -- un élément de plus


	-- Supprimer le noeud associé à la clé dans l'arbre et le sous-arbre dont il est la base.
	-- Paramètres :
		--Arbre l'arbre dans lequel on suprimme un sous arbre.
		--Cle La cle du noeud à la base du sous-arbre
	-- Exception : Cle_Absente_Exception si Clé n'est pas utilisée dans l'arbre.
	procedure Supprimer (Arbre : in out T_Pointeur_Binaire ; Cle : in T_Cle);
		--Post =>  Nombre_Fils (Arbre, Cle) = Nombre_Fils (Arbre, Cle)'Old - 1; -- un élément de moins


	-- Supprimer tous les éléments d'un arbre.
	-- Parametre :
		--Arbre l'arbre que l'on veut suprimmer.
	-- Doit être utilisée dès qu'on sait qu'un arbre ne sera plus utilisé.
	procedure Vider (Arbre : in out T_Pointeur_Binaire) with
		Post => Est_Vide (Arbre);


	-- Affiche l'arbre à l'aide d'une procedure qui permet d'afficher les clés génériques.
	-- Parametre : Arbre l'arbre que l'on veut afficher.
	generic
		with procedure Afficher_Cle (Cle : in T_Cle);
	procedure Afficher_Arbre_Binaire (Arbre : in T_Pointeur_Binaire);

	generic
		with procedure Afficher_Cle(Cle : in T_Cle);
	procedure Afficher_Cle_Binaire (Arbre : in T_Pointeur_Binaire);
	
	-- Procédure qui trouve le pointeur qui pointe vers la cellule dans laquelle se trouve la clé que l'on recherche.
    -- Paramètres:
	-- Arbre l'arbre dans lequel on recherche la clé.
	-- Cle La clé que l'on recherche.
	-- Position Le pointeur qui pointe vers la clé.
    -- Exception : Cle_Absente_Exception si Clé n'est pas utilisée dans l'arbre.
	procedure Rechercher_Position (Arbre : in T_Pointeur_Binaire; Cle : in T_Cle; Pointeur : out T_Pointeur_Binaire);


	procedure Ajouter (Arbre : in out T_Pointeur_Binaire; Cle : in T_Cle);
	

private

    type T_Cellule;

	type T_Pointeur_Binaire is access T_Cellule;

	type T_Cellule is 
	record
        Cle : T_Cle;
        Fils_Gauche : T_Pointeur_Binaire;
		Fils_Droit : T_Pointeur_Binaire;
    end record;

end Arbre_Binaire;