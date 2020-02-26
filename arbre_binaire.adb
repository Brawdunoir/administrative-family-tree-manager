with Ada.Text_IO;
use  Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Arbre_Binaire is

    procedure Free is
	new Ada.Unchecked_Deallocation (T_Cellule, T_Pointeur_Binaire);


    procedure Initialiser(Arbre: out T_Pointeur_Binaire; Cle : in T_Cle) is

        Cellule : T_Pointeur_Binaire;

    begin

        Cellule :=  new T_Cellule;
        Cellule.all.Cle := Cle;
        Cellule.all.Fils_Droit := null;
        Cellule.all.Fils_Gauche:= null;
        Arbre := Cellule;
        
    end Initialiser;

    function Est_Vide (Arbre : in T_Pointeur_Binaire) return Boolean is

    begin
        if Arbre = null then
            return True;
        elsif Arbre.all.Fils_Droit /= null then
            return False;
        elsif Arbre.all.Fils_Gauche /= null then
            return False;
        else 
            return True;
        end if;
    end Est_Vide;

    function Est_Vide_Fils_Droit (Arbre : in T_Pointeur_Binaire) return Boolean is
    begin
        return Arbre.all.Fils_Droit = null;
    end Est_Vide_Fils_Droit;

	function Est_Vide_Fils_Gauche (Arbre : in T_Pointeur_Binaire) return Boolean is
    begin
        return Arbre.Fils_Gauche = null;
    end Est_Vide_Fils_Gauche;

    function Retourner_Fils_Gauche (Arbre : in T_Pointeur_Binaire) return T_Pointeur_Binaire is
    begin
        return Arbre.all.Fils_Gauche;
    end Retourner_Fils_Gauche;

    function Retourner_Cle (Arbre : in T_Pointeur_Binaire) return T_Cle is
    begin
        return Arbre.all.Cle;
    end Retourner_Cle;

    function Retourner_Fils_Droit (Arbre : in T_Pointeur_Binaire) return T_Pointeur_Binaire is
    begin
        return Arbre.all.Fils_Droit;
    end Retourner_Fils_Droit;


    function Nombre_Fils (Arbre : in T_Pointeur_Binaire; Cle : in T_Cle) return Integer is
        Pointeur : T_Pointeur_Binaire;
    begin
        Rechercher_Position (Arbre , Cle, Pointeur);

        return Nombre_Fils_Recu(Pointeur);

    end Nombre_Fils;

    function Est_Present(Arbre : in T_Pointeur_Binaire; Cle : in T_Cle) return Boolean is 
    begin
        if Arbre = null then
            return False;
        elsif Arbre.all.Cle = Cle then
            return True;
        else
            return Est_Present(Arbre.all.Fils_Droit,Cle) or Est_Present(Arbre.all.Fils_Gauche,Cle);
        end if;

    end Est_Present;    


    -- Fonction qui renvoie le nombre le nombre de fils à partir d'un noeud donné.
    -- Paramètres :
	-- Arbre le pointeur qui pointe vers le noeud à partir duquel on veut compter.
    -- But :  Ne pas effectuer plusieurs fois Rechercher_Poisition.
    function Nombre_Fils_Recu (Arbre : in T_Pointeur_Binaire) return Integer is 
    begin
        if Arbre = null then
            return 0;
        else
            return Nombre_Fils_Recu(Arbre.all.Fils_Droit) + Nombre_Fils_Recu(Arbre.all.Fils_Gauche) + 1;
        end if;
    end Nombre_Fils_Recu;


    -- Procédure qui trouve le pointeur qui pointe vers la cellule dans laquelle se trouve la clé que l'on recherche.
    -- Paramètres:
	--      Arbre l'arbre dans lequel on recherche la clé.
	--      Cle La clé que l'on recherche.
	--      Position Le pointeur qui pointe vers la clé
    -- But: Pouvoir lever une exception dans Rechecher_Position si la clé recherchée est absente de l'arbre.
    procedure Trouver_Position (Arbre : in T_Pointeur_Binaire; Cle : in T_Cle; Position : in out T_Pointeur_Binaire) is
    begin

        if Arbre = null then
            null;

        
	    elsif Arbre.all.Cle /= Cle and Position = null  then
		    Trouver_Position (Arbre.all.Fils_Droit, Cle, Position);

        -- Cas possible seulement si Cle = Cle du 1er individu de l'arbre
        elsif Arbre.all.Cle = Cle then
		

            Position := Arbre;

	    else
		    null;
	    end if;    

	    if Arbre = null then 
		    null;
	    elsif Arbre.all.Cle /= Cle and Position = null then

            Trouver_Position (Arbre.all.Fils_Gauche, Cle, Position);

	    elsif Arbre.all.Cle = Cle then

            Position := Arbre;

	    else
		    null;
        end if;

    end Trouver_Position;


    
    procedure Rechercher_Position (Arbre : in T_Pointeur_Binaire; Cle : in T_Cle; Pointeur : out T_Pointeur_Binaire) is
    begin

        Pointeur := null;
        Trouver_Position (Arbre, Cle, Pointeur);

        if Pointeur = null then
            
            raise Cle_Absente_Exception_Bin;

        else
            null;
        end if;

    end Rechercher_Position;


    procedure Ajouter_Fils_Droit (Arbre : in out T_Pointeur_Binaire; Cle_Parent : in T_Cle; Cle_Fils : in T_Cle) is

        Pointeur : T_Pointeur_Binaire;

    begin

        if Est_Present(Arbre,Cle_Fils) then

            raise Cle_Presente_Exception_Bin;

        else

            Rechercher_Position (Arbre, Cle_Parent, Pointeur);

            if Pointeur.all.Fils_Droit /= null then

                raise Emplacement_Reserve_Exception_Bin;
                
            else 

                Ajouter (Pointeur.all.Fils_Droit, Cle_Fils);

            end if;

        end if;



    end Ajouter_Fils_Droit;

    procedure Ajouter_Fils_Gauche (Arbre : in out T_Pointeur_Binaire; Cle_Parent : in T_Cle; Cle_Fils : in T_Cle) is

        Pointeur : T_Pointeur_Binaire;

    begin

        if Est_Present(Arbre,Cle_Fils) then

            raise Cle_Presente_Exception_Bin;
        else

            Rechercher_Position (Arbre, Cle_Parent, Pointeur);

            if Pointeur.all.Fils_Gauche /= null then

                raise Emplacement_Reserve_Exception_Bin;

            else 

                Ajouter (Pointeur.all.Fils_Gauche, Cle_Fils);

            end if;

        end if;



    end Ajouter_Fils_Gauche;

    
    -- Procédure qui ajoute un noeud au bout d'un pointeur.
	-- Paramètre:
    --      Arbre Le pointeur qui va pointer vers le noeud que l'on souhaite créer.
	--      Cle La clé du noeud que l'on ajoute.
    procedure Ajouter (Arbre : in out T_Pointeur_Binaire; Cle : in T_Cle) is
        Cellule : T_Pointeur_Binaire;
    begin
        Cellule :=  new T_Cellule;
        Cellule.all.Cle := Cle;
        Cellule.all.Fils_Droit := null;
        Cellule.all.Fils_Gauche:= null;
        Arbre := Cellule;
    end Ajouter;
    

    -- Procédure qui trouve le pointeur qui pointe vers la cellule dans laquelle se trouve le père de la clé que l'on recherche.
    -- Paramètres:
	-- Arbre l'arbre dans lequel on recherche la clé.
	-- Cle La clé que l'on recherche.
	-- Position Le pointeur qui pointe vers le père de la clé.
    -- Exception : Cle_Absente_Exception_Bin si Clé n'est pas utilisée dans l'arbre.
    -- But :  Besoin pour utiliser vider dans la procedure supprimer.
    procedure Trouver_grand_pere (Arbre : in T_Pointeur_Binaire; Cle : in T_Cle; Position : in out T_Pointeur_Binaire) is
    begin

        if Arbre = null then
            null;
	    elsif Arbre.all.Cle = Cle then
		    Position := Arbre;
	    elsif Arbre.all.Fils_Droit = null and Arbre.all.Fils_Gauche = null then
		    null;
	    elsif Arbre.all.Fils_Droit = null then


		    if Position = null then
		        Trouver_grand_pere(Arbre.all.Fils_Gauche, Cle, Position);
		    else
		        null;
		    end if;

	    elsif Arbre.all.Fils_Gauche = null then

		    if Position = null then
		        Trouver_grand_pere(Arbre.all.Fils_Droit, Cle, Position);
		    else
		        null;
		    end if;

        elsif Arbre.all.Fils_Droit.all.Cle /= Cle and Arbre.all.Fils_Gauche.all.Cle /= Cle and Position = null  then

	        Trouver_grand_pere (Arbre.all.Fils_Droit, Cle, Position);
		    Trouver_grand_pere(Arbre.all.Fils_Gauche, Cle, Position);

        elsif Arbre.all.Fils_Droit.all.Cle = Cle or Arbre.all.Fils_Gauche.all.Cle = Cle then
            Position := Arbre;
	    else
	        null;
	    end if;    

	    
    end Trouver_grand_pere;



    procedure Supprimer (Arbre : in out T_Pointeur_Binaire ; Cle : in T_Cle) is

	    Pointeur : T_Pointeur_Binaire;
        
    begin
	
	    Pointeur := null;
        Trouver_grand_pere (Arbre, Cle, Pointeur);

        if Pointeur = null then
            
            raise Cle_Absente_Exception_Bin;

        elsif Pointeur.all.Cle = Cle then
            
	        Vider(Arbre);

	    else

            if Pointeur.all.Fils_Gauche.all.Cle = Cle then

                Vider(Pointeur.all.Fils_Gauche);
	    
	        else 

		        Vider(Pointeur.all.Fils_Droit);

	        end if;

        end if;
    end Supprimer;


    procedure Vider (Arbre : in out T_Pointeur_Binaire) is
    begin
        if Arbre = null then
            null;
        else
            Vider(Arbre.all.Fils_Droit);
            Vider(Arbre.all.Fils_Gauche);
            Free(Arbre);
        end if;
    end Vider;

    procedure Afficher_Arbre_Binaire (Arbre : in T_Pointeur_Binaire) is
        procedure Indenter(Decalage : in Integer) is
		begin
			for I in 1..Decalage loop
				Put (' ');
			end loop;
		end Indenter;

		-- Afficher un arbre à la profondeur Profondeur et qui à du côté
		-- indiqué (< pour Gauche et > pour droit, - pour la racine).
		procedure Afficher_Profondeur (Arbre: in T_Pointeur_Binaire ; Profondeur : in Integer ; Cote : in Character) is
		begin
			if Arbre = Null then
				Null;
			else
				Indenter (Profondeur * 4);
				Put (Cote & ' ');
				Afficher_Cle (Arbre.all.Cle);
				
				New_Line;

				Afficher_Profondeur (Arbre.all.Fils_Gauche, Profondeur + 1, '<');
				Afficher_Profondeur (Arbre.all.Fils_Droit, Profondeur + 1, '>');
			end if;
		end Afficher_Profondeur;

	begin
		Afficher_Profondeur (Arbre, 0, '-');
	end Afficher_Arbre_Binaire;

    procedure Afficher_Cle_Binaire (Arbre : in T_Pointeur_Binaire) is
    begin
        Afficher_Cle(Arbre.all.Cle);
    end Afficher_Cle_Binaire;
    
end Arbre_Binaire;
