with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;
use  Ada.Text_IO;

package body Arbre_Genealogique is

  
    --package arbre2 is -- On instancie un arbre binaire
      --      new Arbre_Binaire (T_Cle => Integer);
    --use arbre2;

    -- Afficher des entiers (pour les clés).
    procedure Afficher_Entier (N : in Integer) is
    begin
        Put(N, width => 0);
    end Afficher_Entier;

    procedure Afficher_Arbre_Binaire is new arbre.Afficher_Arbre_Binaire (Afficher_Cle => Afficher_Entier);
    procedure Afficher_Cle_Binaire is new arbre.Afficher_Cle_Binaire (Afficher_Cle => Afficher_Entier);

    procedure Initialiser_Gen(Arbre: out T_Arbre_Gen; Reg : out T_Registre;
                                    Nom : in unbounded_string;
                                    Prenom : in unbounded_string;
                                    Jour_N : in Integer;
                                    Mois_N : in Integer;
                                    Annee_N : in Integer;
                                    Sexe : in Character) is
            Donnee : T_Donnee;
            Cle : Integer;
        begin
            Initialiser_Donnee(Donnee, Nom, Prenom, Jour_N, Mois_N, Annee_N, Sexe);
            Creer_Cle (Reg, Cle, Donnee);
            Initialiser (Arbre, Cle);
            Initialiser_Reg (Reg, Cle, Donnee);
        end Initialiser_Gen;


	function Est_Vide_Gen (Arbre : in T_Arbre_Gen) return Boolean is
        begin
            return Est_Vide(Arbre);
        end Est_Vide_Gen;


	function Nombre_Ancetre (Arbre : in T_Arbre_Gen; Cle : in Integer) return Integer is
        begin
            return Nombre_Fils (Arbre, Cle);
        end Nombre_Ancetre;


    procedure Ajouter_Parent (Arbre : in out T_Arbre_Gen; Reg : in out T_Registre; Cle_Individu : in Integer;
                                    Nom : in unbounded_string;
                                    Prenom : in unbounded_string;
                                    Jour_N : in Integer;
                                    Mois_N : in Integer;
                                    Annee_N : in Integer;
                                    Sexe : in Character) is
        Donnee : T_Donnee;
        Cle_Parent : Integer;
    begin
        Initialiser_Donnee (Donnee, Nom, Prenom, Jour_N, Mois_N, Annee_N, Sexe);
        Creer_Cle (Reg, Cle_Parent, Donnee);

        if Sexe = 'F' then
            Ajouter_Fils_Gauche (Arbre, Cle_Individu, Cle_Parent);
        else
            Ajouter_Fils_Droit (Arbre, Cle_Individu, Cle_Parent);
        end if;

        -- on l'insère dans le registre après, car si la clé ou le parent existe déjà, l'exception est propagée
        -- et l'individu n'est pas ajouté au registre
        Inserer (Reg, Cle_Parent, Donnee);

        exception

        when Cle_Presente_Exception_Bin => raise Cle_Presente_Exception_Gen;
        when Cle_Absente_Exception_Bin => raise Cle_Absente_Exception_Gen;
        when Emplacement_Reserve_Exception_Bin => raise Emplacement_Reserve_Exception_Gen;
    end Ajouter_Parent;


    

	procedure Supprimer (Arbre : in out T_Arbre_Gen; Reg : in out T_Registre; Individu : in Integer) is
    begin
        
        Supprimer(Arbre, Individu);
        
        exception

        when Cle_Absente_Exception_Bin => raise Cle_Absente_Exception_Gen;    
    end Supprimer;


	procedure Vider (Arbre : in out T_Arbre_Gen; Reg : in out T_Registre) is
    begin
        Vider(Arbre);
        Vider(Reg);
    end Vider;

    procedure Afficher_Ancetre_Generation_X (Arbre : in T_Arbre_Gen; X : in Integer; compteur : in out Integer) is
    begin
        if compteur > X then
            Afficher_Cle_Genealogique(Arbre);
            New_Line;
        else
            if not Est_Vide_Fils_Droit(T_Pointeur_Binaire(Arbre)) and not Est_Vide_Fils_Gauche(T_Pointeur_Binaire(Arbre)) then
                compteur := compteur + 1;
                Afficher_Ancetre_Generation_X(T_Arbre_Gen(Retourner_Fils_Droit(T_Pointeur_Binaire(Arbre))), X, compteur);
                Afficher_Ancetre_Generation_X(T_Arbre_Gen(Retourner_Fils_Gauche(T_Pointeur_Binaire(Arbre))), X, compteur);
            elsif not Est_Vide_Fils_Gauche(T_Pointeur_Binaire(Arbre)) then
                compteur := compteur +1;
                Afficher_Ancetre_Generation_X(T_Arbre_Gen(Retourner_Fils_Gauche(T_Pointeur_Binaire(Arbre))), X, compteur);
            elsif not Est_Vide_Fils_Droit(T_Pointeur_Binaire(Arbre)) then
                compteur := compteur + 1;
                Afficher_Ancetre_Generation_X(T_Arbre_Gen(Retourner_Fils_Droit(T_Pointeur_Binaire(Arbre))), X, compteur);
            else
                null;
            end if;
        end if;
    end Afficher_Ancetre_Generation_X;

    procedure Ancetre_Generation_X (Arbre : in T_Arbre_Gen; Individu : in Integer; Generation : in Integer) is
        compteur : Integer;
        Pointeur_Individu : T_Pointeur_Binaire;
    begin
        New_Line;
        Put("Les ancêtres de "); Put(Individu, width => 0); Put(" de la génération "); Put(Generation, width => 0); Put( " sont : "); New_Line;
        compteur := 1;
        Rechercher_Position(T_Pointeur_Binaire(Arbre), Individu, Pointeur_Individu);
        Afficher_Ancetre_Generation_X (T_Arbre_Gen(Pointeur_Individu), Generation, compteur);
        
        exception

        when Cle_Absente_Exception_Bin => raise Cle_Absente_Exception_Gen;
    end Ancetre_Generation_X;

    procedure Ancetres_Sur_N_Generations (Arbre : in T_Arbre_Gen; Individu : in Integer; N : in Integer) is 
        compteur : Integer;
        Pointeur_Individu : T_Pointeur_Binaire;
    begin
        New_Line;
        Put("Les ancêtres de "); Put(Individu, width => 0); Put(" sur "); Put(N, width => 0); Put( " génération(s) sont : "); New_Line;
        Rechercher_Position(T_Pointeur_Binaire(Arbre), Individu, Pointeur_Individu);
        -- On affiche chaque génération X à partir d'Individu
        for i in 1..N loop
            compteur := 1;
            Put("Generation "); Put(i); Put(" :"); New_Line;
            Afficher_Ancetre_Generation_X (T_Arbre_Gen(Pointeur_Individu), i, compteur);
        end loop;

        exception

        when Cle_Absente_Exception_Bin => raise Cle_Absente_Exception_Gen;
    end Ancetres_Sur_N_Generations;

    procedure Afficher_Ancetres_Homonymes (P1 : in T_Arbre_Gen; P2 : in T_Arbre_Gen; Reg : in T_Registre) is
    begin
        -- Si P1 et P2 ne sont pas vides...
        if not Est_Vide_Gen(P1) and not Est_Vide_Gen(P2) then
            -- S'ils ont le même nom, on les affiche !
            if Retourner_Nom(Reg, Retourner_Cle(P1)) = Retourner_Nom(Reg, Retourner_Cle(P2)) then
                Put_Line (To_String(Retourner_Nom(Reg, Retourner_Cle(P1))));
            else
                null;
            end if;
        end if;
        
        -- Si les deux sont vides, on a atteint la fin de l'arbre, on s'arrête
        if Est_Vide_Gen(P1) and Est_Vide_Gen(P2) then
            null;
        -- Sinon on va chercher dans tous les ancêtres de P2, si un correspond au premier ancêtre de P1 jusqu'à ce qu'on ai regardé
        -- tous les ancêtres de P2, et alors on passe aux deuxièmes ancêtres de P1 et on recommence
        else
            if not Est_Vide_Fils_Droit(T_Pointeur_Binaire(P2)) and not Est_Vide_Fils_Gauche(T_Pointeur_Binaire(P2)) then
                Afficher_Ancetres_Homonymes (P1, T_Arbre_Gen(Retourner_Fils_Gauche(T_Pointeur_Binaire(P2))), Reg);
                Afficher_Ancetres_Homonymes(P1, T_Arbre_Gen(Retourner_Fils_Droit(T_Pointeur_Binaire(P2))), Reg);
            elsif not Est_Vide_Fils_Gauche(T_Pointeur_Binaire(P2)) then
                Afficher_Ancetres_Homonymes(P1, T_Arbre_Gen(Retourner_Fils_Gauche(T_Pointeur_Binaire(P2))), Reg);
            elsif not Est_Vide_Fils_Droit(T_Pointeur_Binaire(P2)) then
                Afficher_Ancetres_Homonymes(P1, T_Arbre_Gen(Retourner_Fils_Droit(T_Pointeur_Binaire(P2))), Reg);
            else -- On est arrivé à la fin de l'arbre généalogique de P2
                if Est_Vide_Fils_Droit(T_Pointeur_Binaire(P1)) and Est_Vide_Fils_Gauche(T_Pointeur_Binaire(P1)) then
                    null; -- On est arrivé à la fin de l'arbre de P1
                elsif Est_Vide_Fils_Droit(T_Pointeur_Binaire(P1)) then -- On recommence en prenant que le fils gauche 
                    Afficher_Ancetres_Homonymes(T_Arbre_Gen(Retourner_Fils_Gauche(T_Pointeur_Binaire(P1))), P2, Reg);
                elsif Est_Vide_Fils_Gauche(T_Pointeur_Binaire(P1)) then -- Ou bien on recommence en prenant que le fils droit
                    Afficher_Ancetres_Homonymes(T_Arbre_Gen(Retourner_Fils_Droit(T_Pointeur_Binaire(P1))), P2, Reg);
                else -- Ou bien on prend les deux !!
                    Afficher_Ancetres_Homonymes(T_Arbre_Gen(Retourner_Fils_Gauche(T_Pointeur_Binaire(P1))), P2, Reg);
                    Afficher_Ancetres_Homonymes(T_Arbre_Gen(Retourner_Fils_Droit(T_Pointeur_Binaire(P1))), P2, Reg);
                end if;
            end if;
        end if;
    end Afficher_Ancetres_Homonymes;

    procedure Ancetres_Homonymes (Arbre : in T_Arbre_Gen; Reg : in T_Registre; Individu1 : in Integer; Individu2 : in Integer) is
        Pointeur_Individu1 : T_Pointeur_Binaire;
        Pointeur_Individu2 : T_Pointeur_Binaire;
    begin
        Rechercher_Position (T_Pointeur_Binaire(Arbre), Individu1, Pointeur_Individu1);
        Rechercher_Position (T_Pointeur_Binaire(Arbre), Individu2, Pointeur_Individu2);
        Afficher_Ancetres_Homonymes (T_Arbre_Gen(Pointeur_Individu1), T_Arbre_Gen(Pointeur_Individu2), Reg);

        exception

        when Cle_Absente_Exception_Bin => raise Cle_Absente_Exception_Gen;
    end Ancetres_Homonymes;

    procedure Afficher_Orphelins (Arbre : in T_Arbre_Gen) is 
    begin
        if not Est_Vide_Fils_Gauche(T_Pointeur_Binaire(Arbre)) and not Est_Vide_Fils_Droit(T_Pointeur_Binaire(Arbre)) then
            Afficher_Orphelins(T_Arbre_Gen(Retourner_Fils_Gauche(T_Pointeur_Binaire(Arbre))));
            Afficher_Orphelins(T_Arbre_Gen(Retourner_Fils_Droit(T_Pointeur_Binaire(Arbre))));
        elsif not Est_Vide_Fils_Droit(T_Pointeur_Binaire(Arbre)) then
            Afficher_Orphelins(T_Arbre_Gen(Retourner_Fils_Droit(T_Pointeur_Binaire(Arbre))));
        elsif not Est_Vide_Fils_Gauche(T_Pointeur_Binaire(Arbre)) then
            Afficher_Orphelins(T_Arbre_Gen(Retourner_Fils_Gauche(T_Pointeur_Binaire(Arbre))));
        else
            Afficher_Cle_Genealogique(Arbre); New_Line;
        end if;

        exception

        when Cle_Absente_Exception_Bin => raise Cle_Absente_Exception_Gen;
    end Afficher_Orphelins;

    procedure Afficher_Monoparental (Arbre : in T_Arbre_Gen) is 
    begin         
        if not Est_Vide_Fils_Gauche(T_Pointeur_Binaire(Arbre)) and not Est_Vide_Fils_Droit(T_Pointeur_Binaire(Arbre)) then
            Afficher_Monoparental(T_Arbre_Gen(Retourner_Fils_Gauche(T_Pointeur_Binaire(Arbre))));
            Afficher_Monoparental(T_Arbre_Gen(Retourner_Fils_Droit(T_Pointeur_Binaire(Arbre))));
        elsif not Est_Vide_Fils_Droit(T_Pointeur_Binaire(Arbre)) then
            Afficher_Cle_Genealogique(Arbre); New_Line;
            Afficher_Monoparental(T_Arbre_Gen(Retourner_Fils_Droit(T_Pointeur_Binaire(Arbre))));
        elsif not Est_Vide_Fils_Gauche(T_Pointeur_Binaire(Arbre)) then
            Afficher_Cle_Genealogique(Arbre); New_Line;
            Afficher_Monoparental(T_Arbre_Gen(Retourner_Fils_Gauche(T_Pointeur_Binaire(Arbre))));
        else
            null;
        end if;

        exception

        when Cle_Absente_Exception_Bin => raise Cle_Absente_Exception_Gen;
    end Afficher_Monoparental;

    procedure Afficher_Biparental (Arbre : in T_Arbre_Gen) is 
    begin
        if not Est_Vide_Fils_Droit(T_Pointeur_Binaire(Arbre)) and not Est_Vide_Fils_Gauche(T_Pointeur_Binaire(Arbre)) then
            Afficher_Cle_Genealogique(Arbre); New_Line;
            Afficher_Biparental(T_Arbre_Gen(Retourner_Fils_Gauche(T_Pointeur_Binaire(Arbre))));
            Afficher_Biparental(T_Arbre_Gen(Retourner_Fils_Droit(T_Pointeur_Binaire(Arbre))));
        elsif not Est_Vide_Fils_Gauche(T_Pointeur_Binaire(Arbre)) then
            Afficher_Biparental(T_Arbre_Gen(Retourner_Fils_Gauche(T_Pointeur_Binaire(Arbre))));
        elsif not Est_Vide_Fils_Droit(T_Pointeur_Binaire(Arbre)) then
            Afficher_Biparental(T_Arbre_Gen(Retourner_Fils_Droit(T_Pointeur_Binaire(Arbre))));
        else
            null;
        end if;

        exception

        when Cle_Absente_Exception_Bin => raise Cle_Absente_Exception_Gen;
    end Afficher_Biparental;

    
    procedure Afficher_Arbre_Genealogique (Arbre : in T_Arbre_Gen) is
	begin
        Afficher_Arbre_Binaire(T_Pointeur_Binaire(Arbre));
	end Afficher_Arbre_Genealogique;

    procedure Afficher_Cle_Genealogique (Arbre : in T_Arbre_Gen) is
    begin
        Afficher_Cle_Binaire(T_Pointeur_Binaire(Arbre));
    end Afficher_Cle_Genealogique;

end Arbre_Genealogique;
    

