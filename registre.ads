with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

package Registre is

	type T_Registre is private;

	type T_Donnee is private;

	Cle_Presente_Exception_Reg : Exception;	-- une clé est déjà présente dans un Registre
	Cle_Absente_Exception_Reg  : Exception;	-- une clé est absente d'un Registre


	-- Creer une clé Cle à partir d'une donnée Donnee sous le modèle suivant :
    -- Cle = 100 000 000*Mois_N + 1 000 000*Jour_N + 100*Annee_N + X (Avec X entre 0 et 99, en assumant qu'il y a au maximum 100 naissances par jour)
    -- Exemple : clé associée à la donnée Sabrina Tavares 24 Août 1999 F qui est la deuxième personne née le 24 Août 1999.
    -- sera 0824199900
    -- Assure : Cle >= 0
    procedure Creer_Cle (Reg : in T_Registre; Cle : out Integer; Donnee : in T_Donnee) with
        Post => Cle >= 0;

	-- Initialiser une donnée Donnee
	-- Paramètres : Donnee out T_Donnee
	--				Nom in Chaîne de caractères
	--				Prenom in Chaîne de caractères
	--				Jour_N in Entier
	--				Mois_N in Entier
	--				Annee_N in Entier
	--				Sexe in Caractère
    procedure Initialiser_Donnee (  Donnee : out T_Donnee;
                                    Nom : in unbounded_string;
                                    Prenom : in unbounded_string;
                                    Jour_N : in Integer;
                                    Mois_N : in Integer;
                                    Annee_N : in Integer;
                                    Sexe : in Character);


	-- Initialiser un Registre Reg. Le Registre est vide.
	-- Paramètres :	Reg out T_Registre
	procedure Initialiser_Reg(Reg: out T_Registre; Cle : in Integer; Donnee : in T_Donnee);

    -- Supprimer tous les éléments d'un Registre.
	-- Doit être utilisée dès qu'on sait qu'un Registre ne sera plus utilisé.
	-- Paramètres :	Reg in out T_Registre
	-- Assure : Est_Vide(Reg)
	procedure Vider (Reg : in out T_Registre) with
		Post => Est_Vide (Reg);


	-- Renvoie True si le registre est vide, False sinon
	-- Paramètres : 	Reg in T_Registre
	function Est_Vide (Reg : in T_Registre) return Boolean;


	-- Obtenir le nombre d'éléments d'un Registre.
	-- Paramètres :	Reg : in T_Registre
	-- Assure : Taille >= 0 et Taille = 0 si Est_Vide(Reg)
	function Taille (Reg : in T_Registre) return Integer with
		Post => Taille'Result >= 0
			and (Taille'Result = 0) = Est_Vide (Reg);


	-- Insérer la donnée Donnee associée à la clé Cle dans le Registre Reg.
	-- Paramètres :	Reg in out T_Registre
	--				Cle : in Entier
	--				Donnee : in T_Donnee
	-- Exception : Cle_Presente_Exception si Cle est déjà dans Reg.
	-- Assure : La_Donnee(Reg,Cle) = Donnee et Taill(Reg) = Taille(Reg)'Old + 1
	procedure Inserer (Reg : in out T_Registre ; Cle : in Integer ; Donnee : in T_Donnee) with
		Post => La_Donnee (Reg, Cle) = Donnee			-- donnée insérée
				and Taille (Reg) = Taille (Reg)'Old + 1; -- un élément de plus


	-- Modifier la donnée Donnee associée à la clé Cle dans le Registre Reg.
	-- Paramètres :	Reg in out T_Registre
	--				Cle in Entier
	--				Donnee in T_Donnee
	-- Exception : Cle_Absente_Exception si Cle n'est pas utilisée dans Reg
	-- Assure : La_Donnee(Reg,Cle) = Donnee
	procedure Modifier (Reg : in out T_Registre ; Cle : in Integer ; Donnee : in T_Donnee) with
		Post => La_Donnee (Reg, Cle) = Donnee;		-- donnée mise à jour


	-- Supprimer la donnée associée à la clé Cle dans le Registre Reg.
	-- Paramètres : 	Reg in out T_Registre
	--					Cle in Entier
	-- Exception : Cle_Absente_Exception si Cle n'est pas utilisée dans Reg
	-- Assure : Taille(Reg) = Taille(Reg)'Old - 1
	procedure Supprimer (Reg : in out T_Registre ; Cle : in Integer) with
		Post =>  Taille (Reg) = Taille (Reg)'Old - 1; -- un élément de moins


	-- Obtenir la donnée associée à la clé Cle dans le Registre Reg.
	-- Paramètres : 	Reg in T_Registre
	--					Cle in Entier
	-- Exception : Cle_Absente_Exception si Cle n'est pas utilisée dans Reg
	function La_Donnee (Reg : in T_Registre ; Cle : in Integer) return T_Donnee;

	function Retourner_Nom (Reg : in T_Registre; Cle : in Integer) return unbounded_string;


	-- Afficher un Registre Reg dans l'ordre croissant des clés (parcours infixe)
	-- Paramètres :	Reg in T_Registre
	procedure Afficher (Reg : in T_Registre);

	-- Afficher un ABR Abr (en faisant apparaître la strucre grâce à une
	-- indendation et un signe '<', '>', '/' pour indiquer la sous-arbre
	-- gauche, '>' pour un sous arbre droit et '/' pour la racine)
	-- Exemple :
	--
	--  / Cle1 : Valeur1
	--      < Cle2 : Valeur2
	--          > Cle3 : Valeur3
	--      > Cle4 : Valeur 4
	--          < Cle5 : Valeur 5
	--procedure Afficher_Debug (Abr : in T_Registre);

private
	-- Donnée qu'on souhaite ajouter au registre pour un ancêtre donné.
	type T_Donnee is
    record
        Nom : unbounded_string;
        Prenom : unbounded_string;
        Jour_N : Integer;
        Mois_N : Integer;
        Annee_N : Integer;
        Sexe : Character;    
    end record;


	type T_Cellule;

	type T_Registre is access T_Cellule;

	type T_Cellule is
		record
			Cle: Integer;
			Donnee : T_Donnee;
			Sous_Arbre_Gauche : T_Registre;
			Sous_Arbre_Droit : T_Registre;
		end record;

end Registre;
