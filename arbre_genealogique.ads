with Registre;
use Registre;
with Arbre_Binaire;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

package Arbre_Genealogique is

	type T_Arbre_Gen is private;

	Emplacement_Reserve_Exception_Gen : Exception;
	Cle_Absente_Exception_Gen : Exception;
	Cle_Presente_Exception_Gen : Exception;
	

    -- Initialiser un Arbre.
	procedure Initialiser_Gen(Arbre: out T_Arbre_Gen; Reg : out T_Registre;
                                     Nom : in unbounded_string;
                                    Prenom : in unbounded_string;
                                    Jour_N : in Integer;
                                    Mois_N : in Integer;
                                    Annee_N : in Integer;
                                    Sexe : in Character);

	-- Est-ce qu'un Arbre est vide ?
	-- Paramètre :  Arbre L'Arbre que l'on veut tester.
	function Est_Vide_Gen (Arbre : in T_Arbre_Gen) return Boolean;


	-- Obtenir le nombre de fils à partir d'un noeud donné (lui compris).
    -- Paramètres : 
	-- 			Arbre L'Arbre dans lequel on va effectuer le comptage.
	-- 			Cle La clé de l'individu à partir duquel on compte.
    -- Exception: Cle_Absente_Exception si Clé n'est pas utilisée l'Arbre.
	function Nombre_Ancetre (Arbre : in T_Arbre_Gen; Cle : in Integer) return Integer with
		Post => Nombre_Ancetre'Result >= 1;


	-- Insérer un parent à un noeud dans l'Arbre.
	-- Paramètres :
	--			Arbre : l'Arbre dans lequel on ajoute un noeud.
	--			Registre : registre dans lequel on insère les données
	--			Cle_Individu : La cle du noeud auquel on ajoute un Pere
	-- 			Nom : Chaine du qui représente le nom du parent
	--			Prénom : Chaine qui représente le prénom du parent
	--			Jour_N : Entier jour de naissance du parent
	--			Mois_N : Entier mois de naissance du parent
	--			Annee_N : Entier année de naissance du parent
	--			Sexe : Caractère F pour Femelle (ie : Mère) et M pour Mâle (ie : Père)
	-- Exception : Cle_Presente_Exception si la clé est déjà dans l'Arbre.
    procedure Ajouter_Parent (Arbre : in out T_Arbre_Gen; Reg : in out T_Registre; Cle_Individu : in Integer;
                                    Nom : in unbounded_string;
                                    Prenom : in unbounded_string;
                                    Jour_N : in Integer;
                                    Mois_N : in Integer;
                                    Annee_N : in Integer;
                                    Sexe : in Character) with
		Post => Nombre_Ancetre (Arbre, Cle_Individu) = Nombre_Ancetre (Arbre, Cle_Individu)'Old + 1; -- un élément de plus


	-- Supprimer le noeud associé à la clé dans l'Arbre et le sous-Arbre dont il est la base.
	-- Paramètres :
	--			Arbre l'Arbre dans lequel on suprimme un sous Arbre.
	--			Registre registre dans lequel on va supprimer les données
	--			Individu cle du noeud à la base du sous-Arbre
	-- Exception : Cle_Absente_Exception si Clé n'est pas utilisée dans l'Arbre.
	procedure Supprimer (Arbre : in out T_Arbre_Gen; Reg : in out T_Registre; Individu : in Integer);
		--Post =>  Nombre_Ancetre (Arbre, Individu) = Nombre_Ancetre (Arbre, Individu)'Old - 1; -- un élément de moins


	-- Supprimer tous les éléments d'un Arbre.
	-- Parametre :
	--			Arbre l'Arbre que l'on veut suprimmer.
	--			Registre registre que l'on veut supprimer.
	-- Doit être utilisée dès qu'on sait qu'un Arbre et son registre associé ne seront plus utilisés.
	procedure Vider (Arbre : in out T_Arbre_Gen; Reg : in out T_Registre) with
		Post => Est_Vide_Gen (Arbre);


	-- Affiche tous les ancêtres situés à une certaine génération d'un individu donné.
	-- Paramètres :
	--				Arbre : l'Arbre dans lequel on va chercher les ancêtres
	--				Individu : clé de l'individu à partir duquel on commence à compter les générations
	--				Generation : entier qui donne le nombre de génération à compter afin d'afficher les ancêtres
	-- Exception : Cle_Absente_Exception si Individu n'est pas dans l'Arbre.
	procedure Ancetre_Generation_X (Arbre : in T_Arbre_Gen; Individu : in Integer; Generation : in Integer);


	-- Affiche tous les ancêtres des N générations suivantes à partir d'un individu donné.
	-- Paramètres :
	--				Arbre : l'Arbre dans lequel on va chercher les ancêtres
	--				Individu : clé de l'individu à partir duquel on commence à compter les générations
	--				N : entier qui donne le nombre de génération afin d'afficher les ancêtres
	-- Exception : Cle_Absente_Exception si Individu n'est pas dans l'Arbre.
	procedure Ancetres_Sur_N_Generations (Arbre : in T_Arbre_Gen; Individu : in Integer; N : in Integer);


	-- Affiche tous les ancêtres de Individu1 et Individu2 ayant le même nom et prénom.
	-- Paramètres :
	--				Arbre : l'Arbre dans lequel on va chercher les ancêtres
	--				Individu1 : clé du premier individu à partir duquel on commence à regarder les ancêtres
	--				Individu2 : clé du second individu à partir duquel on commence à regarder les ancêtres
	-- Exception : Cle_Absente_Exception si Individu n'est pas dans l'Arbre.
	procedure Ancetres_Homonymes (Arbre : in T_Arbre_Gen; Reg : in T_Registre; Individu1 : in Integer; Individu2 : in Integer);


	-- Affiche tous les individus qui n'ont ni père ni mère.
	-- Paramètre :
	--				Arbre : l'Arbre dans lequel on va chercher les ancêtres
	procedure Afficher_Orphelins (Arbre : in T_Arbre_Gen);


	-- Affiche tous les individus qui n'ont soit pas de père soit pas de mère.
	-- Paramètre :
	--				Arbre : l'Arbre dans lequel on va chercher les ancêtres
	procedure Afficher_Monoparental (Arbre : in T_Arbre_Gen);


	-- Affiche tous les individus qui ont un père et une mère.
	-- Paramètre :
	--				Arbre : l'Arbre dans lequel on va chercher les ancêtres
	procedure Afficher_Biparental (Arbre : in T_Arbre_Gen);

	procedure Afficher_Arbre_Genealogique (Arbre : in T_Arbre_Gen);

	procedure Afficher_Cle_Genealogique (Arbre : in T_Arbre_Gen);


	
private

    package Arbre is -- On instancie un Arbre binaire
            new Arbre_Binaire (T_Cle => Integer);
    use Arbre;

	type T_Arbre_Gen is new T_Pointeur_Binaire;
		
end Arbre_Genealogique;
